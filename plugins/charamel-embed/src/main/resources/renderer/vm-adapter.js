/**
 * VSM ↔ VuppetMaster adapter.
 *
 * Transport-agnostic by design. Two seams:
 *   vsmDispatch(envelope)  — entry: an envelope from VSM (desktop: WebSocket; Android: JS bridge)
 *   vsmFeedback(message)   — exit:  a feedback string to VSM (desktop: WS.send; Android: bridge)
 *
 * Feedback strings sent to VSM:
 *   "id:start" / "id:stop"  — utterance markers (charamel-ws convention; unblocks the ActivityWorker)
 *   "3"                     — bare inline timemark (fires a co-located action)
 *   "vm.ready"              — model fully loaded AND audio unlocked (a real "character ready" signal)
 *   "vm.progress:<0..100>"  — model loading progress
 *   "vm.error:<msg>"        — load error
 *   "vm.heartbeat"          — keep-alive, sent periodically to avoid Jetty's WS idle timeout
 *
 * The engine natively extracts VSM's ${...}$ markers from speak text and reports them via
 * onMarker(name, value). onEnd is a safety net so VSM never hangs if a stop marker is dropped.
 *
 * A third, separate channel exists alongside vsmDispatch/vsmFeedback: the SIA preview panel
 * (parent page) can postMessage {vsmMute: true|false} directly into this frame — see the audio
 * mute section below. Unlike vsmDispatch, this is purely local to this browser tab and never
 * touches VSM/the WebSocket; it exists because the server broadcasts every speak/action command
 * identically to every connected preview session (see JettyTransport.send()), so when two
 * browsers preview the same character at once, each viewer needs to be able to silence audio in
 * their own tab independently of what the other hears.
 */
(function () {
  var cfg = window.VSM_CONFIG || {};
  // URL query params override the server-injected config, so one served page can render different
  // characters — e.g. character.html?appName=Xenia — selected from a screens.json character.srcVar.
  var qs = new URLSearchParams(location.search);
  var licenseKey = qs.get('licenseKey') || cfg.licenseKey;
  var appName    = qs.get('appName')    || cfg.appName;

  var vm = null;
  var ws = null;

  var modelLoaded = false;
  var audioUnlocked = false;
  var readySent = false;

  // ---- local audio mute (client-side only) ----------------------------------
  // The engine exposes no documented mute/volume API and doesn't expose Howler.js (its likely
  // internal audio library) as a page global, so there's no library-specific hook to call. This
  // instead wraps the standard AudioContext constructor BEFORE the vendor engine script loads
  // (loadEngine() below), so whatever audio graph the engine builds — regardless of which library
  // it uses internally — ends up routed through one master GainNode this file controls. Requires
  // no cooperation from the vendor bundle beyond it using the standard Web Audio API, which any
  // engine doing real-time lip-synced audio effectively has to.
  var mMuted = false;
  var mMasterGains = [];
  (function patchAudioContext() {
    var Native = window.AudioContext || window.webkitAudioContext;
    if (typeof Native !== 'function') return;
    function Patched(options) {
      var ctx = (options !== undefined) ? new Native(options) : new Native();
      var master = ctx.createGain();
      master.gain.value = mMuted ? 0 : 1;
      master.connect(ctx.destination);
      mMasterGains.push(master);
      try {
        // Shadow the native (otherwise read-only) .destination getter so anything the engine
        // connects "to the speakers" lands on our gain node first. Standard Web IDL attributes
        // are configurable on the instance by default, so this is expected to succeed; if some
        // browser refuses it, mute simply becomes a no-op for that session rather than an error.
        Object.defineProperty(ctx, 'destination', { get: function () { return master; }, configurable: true });
      } catch (e) { /* see comment above */ }
      return ctx;
    }
    Patched.prototype = Native.prototype;
    window.AudioContext = Patched;
    if (window.webkitAudioContext) window.webkitAudioContext = Patched;
  })();
  window.addEventListener('message', function (e) {
    if (e.data && typeof e.data.vsmMute === 'boolean') {
      mMuted = e.data.vsmMute;
      mMasterGains.forEach(function (g) { g.gain.value = mMuted ? 0 : 1; });
    }
  });

  // ---- transport exit ------------------------------------------------------
  function vsmFeedback(message) {
    if (window.AndroidVSM && typeof window.AndroidVSM.send === 'function') {
      window.AndroidVSM.send(message);            // Android JS bridge (Phase 2)
    } else if (ws && ws.readyState === WebSocket.OPEN) {
      ws.send(message);                            // desktop WebSocket
    }
  }
  // Exposed so the Android host can push envelopes directly into the same code path.
  window.vsmDispatch = vsmDispatch;

  // "Ready" = the character is both visible (model loaded) and audible (audio unlocked).
  // The 100% progress report is deferred to here (not onLoad) so the parent page's "loaded"
  // gate — which enables the authoring-time turn Play buttons — never fires before the engine
  // can actually synthesize audio (audio unlock requires a real click inside this iframe).
  function maybeReady() {
    if (modelLoaded && audioUnlocked && !readySent) {
      readySent = true;
      vsmFeedback('vm.ready');
      try { window.parent.postMessage({ vsmPreviewProgress: 100 }, '*'); } catch (e) {}
    }
  }

  // ---- bone animation ------------------------------------------------------
  // vm.animateBone(boneName, euler, opts) takes its euler in RADIANS (the engine feeds it straight
  // into a THREE.Euler, which is radian-valued). VSM authors are not engineers, so the command
  // surface is in DEGREES and the conversion happens here, at the single point where the engine is
  // actually called — keeping every other layer (SceneFlow command, executor, wire envelope) in the
  // unit an author typed.
  //
  // opts mirrors the engine's own envelope contract:
  //   attack / decay  ms, default 500 each (engine-side)
  //   hold            ms; ABSENT means hold forever (engine's holdInfinite), NOT zero
  //   additive        default true — layer on top of the running idle animation rather than
  //                   replacing the bone's pose outright
  function num(value, fallback) {
    var n = parseFloat(value);
    return isFinite(n) ? n : fallback;
  }

  function animateBoneDegrees(boneName, xDeg, yDeg, zDeg, opts) {
    if (!vm || typeof vm.animateBone !== 'function') {
      console.warn('VSM: animateBone not available on this engine build');
      return;
    }
    var name = boneName || 'Head';
    var toRad = Math.PI / 180;
    var euler = {
      x: num(xDeg, 0) * toRad,
      y: num(yDeg, 0) * toRad,
      z: num(zDeg, 0) * toRad
    };
    var o = {};
    if (opts && opts.attack   !== undefined) o.attack   = num(opts.attack, 500);
    if (opts && opts.decay    !== undefined) o.decay    = num(opts.decay, 500);
    if (opts && opts.hold     !== undefined) o.hold     = num(opts.hold, 0);
    if (opts && opts.additive !== undefined) o.additive = !!opts.additive;
    vm.animateBone(name, euler, o);
  }

  // Which Euler axis moves the head which way, established by testing the live Xenia rig
  // (2026-08-11) rather than assumed from the usual pitch/yaw/roll ordering — on this skeleton the
  // Head bone's local frame does NOT follow it:
  //   x = yaw   (turn / shake, "no")
  //   y = pitch (nod, "yes") — NEGATIVE drops the chin
  //   z = roll  (tilt, ear-to-shoulder) — by elimination, not separately confirmed
  // If a further bone is exposed, re-verify: each bone has its own local frame.
  var BONE_AXES = { yaw: 'x', pitch: 'y', roll: 'z' };

  /**
   * Schedules a symmetric oscillation of one bone axis around its NEUTRAL pose.
   *
   * `amplitude` is the full peak-to-peak excursion, so the head swings ±amplitude/2: amplitude 12
   * travels 6deg out, 12deg across to the other side, then 6deg back to centre. An earlier version
   * animated 0 -> +amplitude -> 0, which only ever moved to one side and read as a lopsided twitch.
   *
   * One cycle of `period` ms is built from quarter-cycle legs, all at the same angular speed:
   *   q       neutral -> first extreme
   *   2q      extreme -> opposite extreme   (repeated 2*repeats-1 times, alternating)
   *   q       last extreme -> neutral       (as a decay, which also frees the envelope)
   * Total is exactly repeats * period, which is what the executor's blocking estimate assumes.
   *
   * Every leg but the last deliberately OMITS hold: the engine reads an absent hold as "hold
   * indefinitely", which parks the bone at that extreme until the next leg takes over. That is what
   * lets the legs chain smoothly — the engine starts each new envelope from the current delta
   * (additive), so there is no snap between legs. The final leg instead uses attack:0 + hold:0 +
   * decay, so it eases from the last extreme back to neutral and then deletes its envelope rather
   * than leaving one alive forever.
   */
  function scheduleOscillation(spec) {
    var half = spec.amplitude / 2;
    var q = spec.period / 4;
    var crossings = 2 * spec.repeats - 1;   // always odd, so the motion ends opposite where it began
    var axis = spec.axis;

    function legAt(delayMs, deg, opts) {
      setTimeout(function () {
        var v = { x: 0, y: 0, z: 0 };
        v[axis] = deg;
        animateBoneDegrees(spec.bone, v.x, v.y, v.z, opts);
      }, delayMs);
    }

    // out to the first extreme
    legAt(0, spec.firstSign * half, { attack: q, additive: true });
    // alternating full-span crossings
    for (var k = 1; k <= crossings; k++) {
      legAt(q + (k - 1) * 2 * q,
            spec.firstSign * half * (k % 2 === 0 ? 1 : -1),
            { attack: 2 * q, additive: true });
    }
    // ease the last extreme back to neutral (crossings is odd => last target is -firstSign*half)
    legAt(q + crossings * 2 * q,
          -spec.firstSign * half,
          { attack: 0, hold: 0, decay: q, additive: true });
  }

  // ---- transport entry -----------------------------------------------------
  function vsmDispatch(env) {
    if (!vm) { console.warn('VSM: engine not ready, dropping', env); return; }
    switch (env.cmd) {
      case 'speak':
        // Authoring-time preview: lets the parent page (script editor) disable every turn's
        // Play button while this character is actually speaking, so a click mid-utterance can't
        // start an overlapping second speak — the REST dispatch itself returns immediately and
        // can't signal true completion on its own.
        try { window.parent.postMessage({ vsmSpeaking: true }, '*'); } catch (e) {}
        vm.speakCommand({ text: env.text, voice: env.voice }, {
          onStart: function () { /* onMarker delivers the start bracket */ },
          onEnd: function (success, error) {
            if (!success) console.warn('VSM: speak ended with error', error);
            try { window.parent.postMessage({ vsmSpeaking: false }, '*'); } catch (e) {}
            vsmFeedback(env.id + ':stop');         // safety net (idempotent on the VSM side)
          },
          onMarker: function (name, value) {
            name = String(name).replace(/^['"]+|['"]+$/g, ''); // strip quotes from single-token names
            vsmFeedback((value !== undefined && value !== null) ? (name + ':' + value) : name);
          }
        });
        break;

      case 'background': {
        // The avatar canvas is transparent, so the page backdrop shows behind it.
        var c = env.color || '';
        document.body.style.background = c;
        var w = document.getElementById('vuppetmaster');
        if (w) w.style.background = c;
        break;
      }

      case 'emotion': {
        var opts = {};
        if (env.intensity !== undefined) opts.intensity = env.intensity;
        if (env.attack    !== undefined) opts.attack    = env.attack;
        if (env.hold      !== undefined) opts.hold      = env.hold;
        if (env.decay     !== undefined) opts.decay     = env.decay;
        vm.setEmotion(env.type, opts);
        break;
      }

      case 'clearEmotion':
        vm.clearEmotion();
        break;

      case 'bone': {
        animateBoneDegrees(env.bone, env.x, env.y, env.z, env);
        break;
      }

      case 'nod': {
        scheduleOscillation({
          bone: env.bone || 'Head',
          axis: BONE_AXES.pitch,
          firstSign: -1,    // negative pitch drops the chin, so a nod starts downward
          amplitude: num(env.amplitude, 12),
          repeats: Math.max(1, Math.round(num(env.repeats, 2))),
          period: Math.max(1, num(env.period, 400))
        });
        break;
      }

      case 'shake': {
        scheduleOscillation({
          bone: env.bone || 'Head',
          axis: BONE_AXES.yaw,
          // Which side a shake starts on carries no meaning, unlike a nod's downward start.
          firstSign: -1,
          amplitude: num(env.amplitude, 16),
          repeats: Math.max(1, Math.round(num(env.repeats, 2))),
          period: Math.max(1, num(env.period, 400))
        });
        break;
      }

      default:
        console.warn('VSM: unknown cmd', env);
    }
  }

  // ---- engine + connection -------------------------------------------------
  function initEngine() {
    // Second constructor arg carries the load lifecycle callbacks.
    vm = new Vuppetmaster.VuppetMaster({
      windowElement: document.getElementById('vuppetmaster'),
      licenseKey: licenseKey,
      appName: appName
    }, {
      onProgress: function (p) {
        vsmFeedback('vm.progress:' + Math.round(p));
        var ov = document.getElementById('overlay');
        if (ov && !modelLoaded) ov.textContent = 'Charakter lädt … ' + Math.round(p) + '%';
        // Authoring-time preview: the parent page (the floating preview panel) renders this as a
        // progress fill on its script-toolbar toggle button. Same-tab postMessage — no backend
        // round-trip needed since the iframe and the toolbar button share one browser tab.
        // Capped below 100 here — true 100%/ready is only reported once audio is unlocked too
        // (see maybeReady), so the parent's "loaded" gate can't enable Play before speak() works.
        try { window.parent.postMessage({ vsmPreviewProgress: Math.min(99, Math.round(p)) }, '*'); } catch (e) {}
      },
      onLoad: function () {
        modelLoaded = true;
        var ov = document.getElementById('overlay');
        if (ov) ov.textContent = 'Klicken zum Starten';
        maybeReady();
      },
      onError: function (e) {
        console.error('VSM: engine load error', e);
        vsmFeedback('vm.error:' + (e && e.message ? e.message : String(e)));
      }
    });
  }

  // Jetty closes a WS with no traffic for 10 minutes (see JettyTransport's setIdleTimeout) — a
  // preview panel left open while the user reads/edits without speaking would otherwise idle out,
  // forcing vm-adapter.js's reconnect loop below to redial from scratch on the next speak attempt
  // (observed 2026-07-17: "WebSocket error: Connection Idle Timeout" in the server log, followed by
  // repeated failed reconnect attempts before the client noticed and recovered). Well under 10
  // minutes so a single missed beat still leaves margin.
  var HEARTBEAT_INTERVAL_MS = 120000;

  function startHeartbeat() {
    setInterval(function () { vsmFeedback('vm.heartbeat'); }, HEARTBEAT_INTERVAL_MS);
  }

  function connect() {
    // Android: the host drives this page through the JS bridge (window.AndroidVSM /
    // window.vsmDispatch), so there is no WebSocket server to connect to — skip it.
    if (window.AndroidVSM && typeof window.AndroidVSM.send === 'function') {
      console.log('VSM: Android bridge present, skipping WebSocket');
      return;
    }
    // Match the page scheme: wss when the character page is served over HTTPS
    // (--secure mode), ws otherwise. Same host/port as the page (self-hosted).
    var wsProto = (location.protocol === 'https:') ? 'wss' : 'ws';
    // Forward the page's own ?vsmPreview=1 (set only on the authoring-time SIA preview panel's
    // iframe src) onto the WS handshake — the query string doesn't travel with the WebSocket
    // URL on its own, and the server (JettyTransport) needs it there to tag this specific
    // session so a real SceneFlow run can mute just the preview without muting other viewers.
    var wsQuery = qs.get('vsmPreview') ? '?vsmPreview=1' : '';
    // Nginx-routed deployments (VSM's inner-nginx dynamic plugin routing, doc/vsm-workspace-
    // platform-plan.md Phase 5): the browser may not be able to reach this page's raw port
    // directly, so connect through the SAME path prefix the page was itself loaded under
    // instead of the absolute /ws path. cfg.pathPrefix is server-injected (see vsm-config.js,
    // JettyTransport.start()) only when VSM_PLUGIN_PATH_PREFIX_ENABLED is on — empty (falls
    // through to the pre-existing /ws behavior, untouched) in every other deployment mode.
    // charamel-embed's WS lives on the same single "port" property as the page itself (unlike
    // htmlgui-ws's separate html_port/ws_port), so the path segment is always literally "port".
    var wsPath = cfg.pathPrefix ? (cfg.pathPrefix + 'port/ws') : '/ws';
    ws = new WebSocket(wsProto + '://' + location.host + wsPath + wsQuery);
    ws.onopen  = function () { console.log('VSM: WebSocket open'); };
    ws.onmessage = function (e) {
      try { vsmDispatch(JSON.parse(e.data)); }
      catch (err) { console.error('VSM: bad envelope', err, e.data); }
    };
    ws.onclose = function () { setTimeout(connect, 1000); };  // auto-reconnect
    ws.onerror = function () {};
  }

  function loadEngine() {
    var s = document.createElement('script');
    s.src = cfg.engineUrl;
    s.onload = function () { initEngine(); connect(); startHeartbeat(); };
    s.onerror = function () { console.error('VSM: failed to load engine', cfg.engineUrl); };
    document.head.appendChild(s);
  }

  // Audio MUST be unlocked by a real user gesture in THIS iframe's document — allow="autoplay" does
  // not resume a covered cross-origin frame's AudioContext (confirmed: state stays "suspended").
  // The overlay click bubbles to document, where the engine's Howler unlock listener resumes the
  // context. When embedded, we also tell the parent renderer we're unlocked so it can drop this
  // iframe behind the screen controls (it was rendered on top so this overlay was reachable).
  var embedded = (window.self !== window.top);
  var overlay = document.getElementById('overlay');
  if (overlay) {
    overlay.addEventListener('click', function () {
      overlay.style.display = 'none';
      audioUnlocked = true;
      if (embedded) { try { parent.postMessage({ vsmCharacter: 'unlocked' }, '*'); } catch (e) {} }
      maybeReady();
    }, { once: true });
  } else {
    audioUnlocked = true;
  }
  loadEngine();
})();
