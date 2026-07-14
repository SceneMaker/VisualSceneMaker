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
 *
 * The engine natively extracts VSM's ${...}$ markers from speak text and reports them via
 * onMarker(name, value). onEnd is a safety net so VSM never hangs if a stop marker is dropped.
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

  // ---- transport entry -----------------------------------------------------
  function vsmDispatch(env) {
    if (!vm) { console.warn('VSM: engine not ready, dropping', env); return; }
    switch (env.cmd) {
      case 'speak':
        vm.speakCommand({ text: env.text, voice: env.voice }, {
          onStart: function () { /* onMarker delivers the start bracket */ },
          onEnd: function (success, error) {
            if (!success) console.warn('VSM: speak ended with error', error);
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
    ws = new WebSocket(wsProto + '://' + location.host + '/ws' + wsQuery);
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
    s.onload = function () { initEngine(); connect(); };
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
