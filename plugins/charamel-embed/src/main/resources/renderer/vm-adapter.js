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
  function maybeReady() {
    if (modelLoaded && audioUnlocked && !readySent) {
      readySent = true;
      vsmFeedback('vm.ready');
    }
  }

  // ---- transport entry -----------------------------------------------------
  function vsmDispatch(env) {
    if (!vm) { console.warn('VSM: engine not ready, dropping', env); return; }
    switch (env.cmd) {
      case 'speak':
        vm.speak({ voice: env.voice, text: env.text }, {
          onStart: function () { /* onMarker delivers the start bracket; nothing needed here */ },
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
      // Phase 3: 'animation', 'emotion', 'gaze', 'head'
      default:
        console.warn('VSM: unknown cmd', env);
    }
  }

  // ---- engine + connection -------------------------------------------------
  function initEngine() {
    // Second constructor arg carries the load lifecycle callbacks.
    vm = new Vuppetmaster.VuppetMaster({
      windowElement: document.getElementById('vuppetmaster'),
      licenseKey: cfg.licenseKey,
      appName: cfg.appName
    }, {
      onProgress: function (p) {
        vsmFeedback('vm.progress:' + Math.round(p));
        var ov = document.getElementById('overlay');
        if (ov && !modelLoaded) ov.textContent = 'Charakter lädt … ' + Math.round(p) + '%';
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
    ws = new WebSocket('ws://' + location.host + '/ws');
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

  // Overlay = audio-unlock gesture (standalone only). When embedded in htmlgui-ws, the screens
  // iframe sits on top so the overlay can't be clicked — but the parent grants this iframe
  // allow="autoplay", so audio plays without a per-iframe gesture. Hide the overlay and treat
  // audio as ready. If no overlay exists (e.g. Android kiosk), likewise assume audio is usable.
  var embedded = (window.self !== window.top);
  var overlay = document.getElementById('overlay');
  if (embedded) {
    if (overlay) overlay.style.display = 'none';
    audioUnlocked = true;
  } else if (overlay) {
    overlay.addEventListener('click', function () {
      overlay.style.display = 'none';
      audioUnlocked = true;
      maybeReady();
    }, { once: true });
  } else {
    audioUnlocked = true;
  }
  loadEngine();
})();
