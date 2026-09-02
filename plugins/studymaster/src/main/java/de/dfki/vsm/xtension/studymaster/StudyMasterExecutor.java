package de.dfki.vsm.xtension.studymaster;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.NodeExecutedEvent;
import de.dfki.vsm.event.event.NodeStartedEvent;
import de.dfki.vsm.event.event.NodeTerminatedEvent;
import de.dfki.vsm.event.event.SceneExecutedEvent;
import de.dfki.vsm.event.event.TurnExecutedEvent;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.sceneflow.chart.AliasNode;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SceneFlow;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.tpl.Tuple;
import io.javalin.Javalin;
import io.javalin.websocket.WsContext;

import org.json.JSONArray;
import org.json.JSONObject;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * Wizard-of-Oz remote control. A single authenticated wizard connects over WebSocket to observe
 * the running flow — every concurrently active top-level flow/thread, each with its own active
 * node, scene and turn — and set author-wired SceneFlow variables, either spontaneously ("set")
 * or blockingly when the flow calls request(var, prompt).
 */
public class StudyMasterExecutor extends ActivityExecutor implements EventListener {

    private static final String WS_PORT_PROP = "ws_port";
    private static final int WS_PORT_DEFAULT = 9091;
    private static final String WIZARD_KEY_PROP = "wizard_key";
    private static final String CONTROLS_FILE_PROP = "controls_file";
    private static final String CONTROLS_FILE_DEFAULT = "wizard-controls.json";

    private Javalin mHttpServer;
    private String mWizardKey;
    private Map<String, ControlSpec> mControls = new LinkedHashMap<>();

    // The single connected-and-authenticated wizard, if any. Replaced (old socket closed) on a
    // fresh successful auth, never rejected — see design decision: reconnect replaces.
    private WsContext mWizardSocket;

    // Outstanding blocking request(s). In practice at most one at a time given "author-wired
    // variables only, one wizard" scope, but keyed by requestId like studymaster-web's
    // mActivityWorkerMap so a stray late reply can't unblock the wrong activity.
    private final Map<String, ActivityWorker> mActivityWorkerMap = new HashMap<>();
    private JSONObject mPendingRequest; // null when nothing outstanding

    // One entry per top-level flow, keyed by its name, holding a running history of the nodes it
    // has visited so far (and, per node, the last scene played there and every turn spoken during
    // that play) — a log, not a single "current" snapshot, so a node's info stays visible after
    // the flow has moved on. threadIdentity() -> {flow, current node id} tracks just enough to
    // attribute a SceneExecutedEvent/TurnExecutedEvent (which don't carry the node) to the right
    // node visit. All maps here are mutated from whatever interpreter thread fires the underlying
    // event, so access is synchronized on mFlowStates for all of them.
    private final Map<String, FlowState> mFlowStates = new LinkedHashMap<>();
    // Keyed by the actual Process/Thread object, not its name — see threadIdentity()'s docs for
    // why a name isn't a safe key.
    private final Map<Thread, String> mThreadFlow = new HashMap<>();
    private final Map<Thread, String> mThreadNode = new HashMap<>();
    // The single most recently fully-vacated flow (no thread maps to it in mThreadFlow any more)
    // and the flow whose arrival caused that — kept visible as bridging context for how the current
    // flow was reached (e.g. "Name" stays visible right after "Repeat after me" starts). Tracked at
    // the FLOW level, not per-thread: several different Process identities can all be attributed to
    // the same flow (e.g. via the alias-inheritance below), so only "does ANY thread still map to
    // this flow" is a reliable signal — a single thread's own transition isn't, since some other
    // thread already contributing to the same flow may still be active. Cleared once the successor
    // flow has shown at least two distinct node visits of its own (see the NodeStartedEvent
    // handler) — proof it's running on its own now, the same "settling" signal as before, just
    // measured on the flow's accumulated history instead of one thread's activity.
    private String mLastRetiredFlow;
    private String mLastRetiredFlowSuccessor;
    // Recovers a PlayerWorker's owning Process from the name prefix threadIdentity() reads off it —
    // always the most recent Process to claim that name, so an earlier, now-dead Process that
    // happened to start at the same node can't be mistaken for the current one.
    private final Map<String, Thread> mNameToProcess = new HashMap<>();
    // Safety net for updateCurrentNodeVisit(): the most recently started node's visit, session-wide,
    // used only if the per-thread lookup above comes up empty — better to attribute a scene/turn to
    // a slightly-stale node than to silently drop it.
    private NodeVisit mLastVisit;

    // ids of every top-level SuperNode that's ever targeted by an AliasNode somewhere in this
    // project (e.g. "Handle Speech Input", reused from inside "Name" via a nested alias) —
    // collected once at launch. See topLevelFlowOf()'s caller in update() for why this matters:
    // a node reached by diving into one of these mid-flow (e.g. asking the name handing off into
    // Handle Speech Input's shared subtree) should stay attributed to whatever flow got it there,
    // not be reassigned to the shared component's own identity.
    private final Set<String> mAliasCanonicalIds = new HashSet<>();

    public StudyMasterExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        mWizardKey = mConfig.getProperty(WIZARD_KEY_PROP, "");
        if (mWizardKey.isEmpty()) {
            mLogger.failure("StudyMaster: no wizard_key configured, refusing to start "
                    + "(would otherwise accept any wizard with no authentication).");
            return;
        }

        loadControls();
        collectAliasCanonicalIds(mProject.getSceneFlow(), mAliasCanonicalIds);
        mProject.getEventDispatcher().register(this);

        final int port = Integer.parseInt(mConfig.getProperty(WS_PORT_PROP, String.valueOf(WS_PORT_DEFAULT)));
        mHttpServer = Javalin.create(config -> config.staticFiles.add(staticFiles -> {
            staticFiles.directory = "/wizard-web";
        })).start(port);
        mHttpServer.ws("/ws", ws -> {
            ws.onMessage(this::onMessage);
            ws.onClose(this::onClose);
            ws.onError(ctx -> mLogger.failure("StudyMaster WS error: " + ctx.error()));
        });
        mLogger.message("StudyMaster listening on port " + port
                + " (" + mControls.size() + " wizard-controllable variable(s))");
    }

    @Override
    public void unload() {
        mProject.getEventDispatcher().remove(this);

        synchronized (this) {
            if (mPendingRequest != null) {
                sendToWizard(new JSONObject()
                        .put("type", "requestCancelled")
                        .put("requestId", mPendingRequest.optString("requestId")));
                mPendingRequest = null;
            }
        }
        synchronized (mActivityWorkerMap) {
            if (!mActivityWorkerMap.isEmpty()) {
                mActivityWorkerMap.clear();
                mActivityWorkerMap.notifyAll(); // release any ActivityWorker still blocked in execute()
            }
        }
        synchronized (this) {
            if (mWizardSocket != null && mWizardSocket.session.isOpen()) {
                mWizardSocket.session.close();
            }
            mWizardSocket = null;
        }
        if (mHttpServer != null) {
            mHttpServer.stop();
        }
    }

    // ---- SceneFlow-facing: the blocking request(var, prompt) command --------------------------

    @Override
    public void execute(AbstractActivity activity) {
        final String actionName = activity.getName();
        if (!"request".equals(actionName)) {
            mLogger.warning("StudyMaster: unknown action '" + actionName + "'");
            return;
        }

        final LinkedList<ActionFeature> features = activity.getFeatures();
        final String var = getFeatureValueNoQuotes("var", features);
        final String promptOverride = getFeatureValueNoQuotes("prompt", features);

        if (var.isEmpty()) {
            mLogger.failure("StudyMaster: request() needs a 'var' feature, e.g. request([var='nextTopic'])");
            return;
        }
        final ControlSpec spec = mControls.get(var);
        if (spec == null) {
            mLogger.failure("StudyMaster: request() var '" + var + "' is not declared in the controls manifest");
            return;
        }

        final String requestId = UUID.randomUUID().toString();
        final JSONObject requestMsg = new JSONObject()
                .put("type", "request")
                .put("requestId", requestId)
                .put("var", var)
                .put("prompt", promptOverride.isEmpty() ? spec.label : promptOverride)
                .put("kind", spec.kind);
        if (spec.options != null) {
            requestMsg.put("options", spec.options);
        }

        synchronized (this) {
            mPendingRequest = requestMsg;
        }
        sendToWizard(requestMsg);

        // Force blocking unconditionally rather than relying on the ambient default, which
        // differs between a PlayAction node command (blocking) and an inline SceneScript action
        // (fire-and-forget) — see project_vsm_step_completion_semantics.
        activity.setType(AbstractActivity.Type.blocking);

        synchronized (mActivityWorkerMap) {
            final ActivityWorker worker = (ActivityWorker) Thread.currentThread();
            mActivityWorkerMap.put(requestId, worker);
            mLogger.message("StudyMaster: blocking on request " + requestId + " for var '" + var + "' (no timeout)");
            while (mActivityWorkerMap.containsKey(requestId)) {
                try {
                    mActivityWorkerMap.wait();
                } catch (InterruptedException e) {
                    mLogger.failure(e.toString());
                }
            }
            mLogger.message("StudyMaster: unblocked on request " + requestId);
        }
    }

    // ---- WebSocket wire protocol ---------------------------------------------------------------

    private void onMessage(io.javalin.websocket.WsMessageContext ctx) {
        final JSONObject msg;
        try {
            msg = new JSONObject(ctx.message());
        } catch (Exception e) {
            mLogger.warning("StudyMaster: malformed frame ignored: " + e);
            return;
        }
        final String type = msg.optString("type", "");

        if ("auth".equals(type)) {
            handleAuth(ctx, msg);
            return;
        }

        synchronized (this) {
            // TODO: verify Javalin 6.7's WsContext exposes a stable per-connection identity to
            // compare against (session object identity used here as the working assumption).
            if (mWizardSocket == null || !mWizardSocket.session.equals(ctx.session)) {
                sendError(ctx, "not_authenticated", "Send 'auth' first");
                return;
            }
        }

        switch (type) {
            case "set":
                handleSet(msg);
                break;
            case "reply":
                handleReply(msg);
                break;
            default:
                sendError(ctx, "unknown_type", "Unknown message type '" + type + "'");
        }
    }

    private void handleAuth(io.javalin.websocket.WsMessageContext ctx, JSONObject msg) {
        final String key = msg.optString("key", "");
        if (mWizardKey.isEmpty() || !mWizardKey.equals(key)) {
            ctx.send(new JSONObject().put("type", "authResult").put("ok", false).put("reason", "invalid_key").toString());
            ctx.session.close();
            return;
        }

        synchronized (this) {
            if (mWizardSocket != null && mWizardSocket.session.isOpen() && !mWizardSocket.session.equals(ctx.session)) {
                mLogger.message("StudyMaster: new wizard connection replaces the existing one");
                mWizardSocket.session.close();
            }
            mWizardSocket = ctx;
        }

        ctx.send(new JSONObject().put("type", "authResult").put("ok", true).toString());
        ctx.send(buildSnapshot().toString());
    }

    private void onClose(io.javalin.websocket.WsCloseContext ctx) {
        synchronized (this) {
            if (mWizardSocket != null && mWizardSocket.session.equals(ctx.session)) {
                mLogger.message("StudyMaster: wizard disconnected");
                mWizardSocket = null;
                // Deliberately not cancelling mPendingRequest here: a reconnecting wizard should
                // see it again via the next snapshot's pendingRequest field.
            }
        }
    }

    private void handleSet(JSONObject msg) {
        final String var = msg.optString("var", "");
        if (!mControls.containsKey(var)) {
            sendError(mWizardSocket, "unknown_var", "Variable '" + var + "' is not wizard-controllable");
            return;
        }
        applyVariable(var, msg.opt("value"));
    }

    private void handleReply(JSONObject msg) {
        final String requestId = msg.optString("requestId", "");
        final String var;
        synchronized (this) {
            if (mPendingRequest == null || !requestId.equals(mPendingRequest.optString("requestId"))) {
                sendError(mWizardSocket, "stale_reply", "No matching pending request for id " + requestId);
                return;
            }
            var = mPendingRequest.optString("var");
        }

        applyVariable(var, msg.opt("value"));

        synchronized (mActivityWorkerMap) {
            if (mActivityWorkerMap.remove(requestId) != null) {
                mActivityWorkerMap.notifyAll();
            }
        }
        synchronized (this) {
            mPendingRequest = null;
        }
    }

    /**
     * ActionFeature.getVal() deliberately retains surrounding single-quotes if the author used
     * them (e.g. request([var='nextTopic']) yields "'nextTopic'") — ActivityExecutor's inherited
     * getActionFeatureValue() helper uses that raw form, which is right for plugins that want the
     * quoting untouched but wrong here. getValNoQuotes() is the strip-quotes counterpart; there's
     * no bracket/list equivalent of getActionFeatureValue() built on top of it, so it's redone here.
     */
    private static String getFeatureValueNoQuotes(String name, LinkedList<ActionFeature> features) {
        for (ActionFeature f : features) {
            if (f.getKey().equalsIgnoreCase(name)) {
                return f.getValNoQuotes();
            }
        }
        return "";
    }

    private void applyVariable(String var, Object value) {
        // TODO: coerce by ControlSpec.kind (boolean/number/choice/string) instead of a blanket
        // toString() — RunTimeProject.setVariable has typed overloads (String/boolean/int/float).
        mProject.setVariable(var, String.valueOf(value));
        broadcastVariableChanged(var, value);
    }

    // ---- Runtime event -> wizard broadcasts -----------------------------------------------------

    @Override
    public void update(EventObject event) {
        if (event instanceof NodeStartedEvent) {
            final BasicNode rawNode = ((NodeStartedEvent) event).getNode();
            if (rawNode instanceof SceneFlow) {
                // Interpreter.start() fires exactly this once per fresh run, for a brand-new root
                // Process — the only observable "a run just (re)started" signal (there is no
                // dedicated restart event). Wipe everything so a Stop+Play cycle doesn't leave the
                // previous run's flows on screen.
                synchronized (mFlowStates) {
                    mFlowStates.clear();
                    mThreadFlow.clear();
                    mThreadNode.clear();
                    mLastRetiredFlow = null;
                    mLastRetiredFlowSuccessor = null;
                    mNameToProcess.clear();
                    mLastVisit = null;
                }
                broadcastActiveFlows();
                return;
            }

            final BasicNode node = resolveAlias(rawNode);
            final JSONObject nodeJson = new JSONObject()
                    .put("id", node.getId())
                    .put("name", node.getName())
                    .put("comment", node.getComment());
            // Deliberately walked from the RAW node, not the alias-resolved one: an alias used
            // *inside* another flow (e.g. "Name" handing off to "Handle Speech Input" partway
            // through asking for the name) must keep resolving to its actual structural parent
            // ("Name") here, not jump straight to the shared component's own top-level identity —
            // that reassignment only makes sense when the alias itself IS the top-level entry
            // point (see the isAliasTarget handling below), not when it's invoked from within.
            final BasicNode structuralTopLevel = topLevelFlowOf(rawNode);
            final Thread identity = threadIdentity();
            synchronized (mFlowStates) {
                final String previousFlowForThread = mThreadFlow.get(identity);
                final boolean isAliasTarget = mAliasCanonicalIds.contains(structuralTopLevel.getId());
                final String flowName;
                if (previousFlowForThread != null && isAliasTarget) {
                    // This thread was already somewhere; it's now diving into a shared component
                    // reached via alias (Handle Speech Input's actual subtree is the SAME shared
                    // node objects regardless of which alias led there, so there's no way to tell
                    // structurally that this dive came from "Name" — only the thread's own history
                    // knows that). Stay attributed to wherever it already was.
                    flowName = previousFlowForThread;
                } else if (previousFlowForThread == null && isAliasTarget && identity instanceof Process) {
                    // A brand-new thread whose very first observed node already lands inside a
                    // shared component (e.g. a child process spawned specifically to run "Handle
                    // Speech Input" on behalf of the flow that invoked it) — inherit the spawning
                    // thread's current flow, if it has one, rather than the component's own name.
                    final Process parent = ((Process) identity).getParentThread();
                    final String parentFlow = parent != null ? mThreadFlow.get(parent) : null;
                    flowName = parentFlow != null ? parentFlow : resolveAlias(structuralTopLevel).getName();
                } else {
                    // A genuine top-level entry: either not an alias target at all (a normal
                    // SuperNode, e.g. "Name" itself), or a fresh/independent thread reaching an
                    // alias target directly (e.g. a fork spawning its own "Handle Speech Input"
                    // instance) — group it under the shared component's own canonical identity.
                    flowName = resolveAlias(structuralTopLevel).getName();
                }

                // DIAGNOSTIC (temporary): full detail on every transition, so a stuck flow can be
                // traced to exactly which decision produced it.
                if (previousFlowForThread == null) {
                    mLogger.message("StudyMaster: thread " + System.identityHashCode(identity)
                            + " started fresh in flow '" + flowName + "' (node=" + rawNode.getId()
                            + " structuralTop=" + structuralTopLevel.getId() + " isAliasTarget=" + isAliasTarget + ")");
                } else if (!previousFlowForThread.equals(flowName)) {
                    mLogger.message("StudyMaster: thread " + System.identityHashCode(identity)
                            + " moved from flow '" + previousFlowForThread + "' to '" + flowName
                            + "' (node=" + rawNode.getId() + " structuralTop=" + structuralTopLevel.getId()
                            + " isAliasTarget=" + isAliasTarget + ")");
                }

                mThreadFlow.put(identity, flowName);
                mThreadNode.put(identity, node.getId());

                if (previousFlowForThread != null && !previousFlowForThread.equals(flowName)
                        && !mThreadFlow.containsValue(previousFlowForThread)) {
                    // The flow just left now has no thread left in it at all (checked AFTER the put
                    // above, so this thread's own new flow doesn't count) — remember it as bridging
                    // context for how flowName was reached, e.g. "Name" stays visible right as
                    // "Repeat after me" starts. Tracked at the flow level rather than per-thread:
                    // several different Process identities can be attributed to the same flow (see
                    // the alias-inheritance above), so only "does ANY thread still map to this flow"
                    // is a reliable signal that it's actually vacated.
                    mLastRetiredFlow = previousFlowForThread;
                    mLastRetiredFlowSuccessor = flowName;
                }

                final FlowState flowState = mFlowStates.computeIfAbsent(flowName, FlowState::new);
                // computeIfAbsent, not put: a revisited node (e.g. a loop back-edge) keeps its
                // place in the flow's history instead of jumping to the end or duplicating.
                final NodeVisit visit = flowState.visits.computeIfAbsent(node.getId(), id -> new NodeVisit());
                visit.node = nodeJson;
                mLastVisit = visit;

                if (flowName.equals(mLastRetiredFlowSuccessor) && flowState.visits.size() >= 2) {
                    // flowName has now shown a second distinct node of its own — proof it's running
                    // on its own, so the bridging predecessor has served its purpose. Without this,
                    // a flow that settles into a stable loop right after one transition (e.g.
                    // "Start and wait" -> "Handling User speaking", which then just loops on VAD
                    // checks forever) would never produce another transition to retire the bridge,
                    // leaving it stuck in the visible set permanently.
                    mLastRetiredFlow = null;
                    mLastRetiredFlowSuccessor = null;
                }
            }
            broadcastActiveFlows();

        } else if (event instanceof NodeTerminatedEvent) {
            // A thread that terminates without ever transitioning again (e.g. a fork's parent,
            // whose only job was spawning children) must stop counting as "current" for its flow —
            // otherwise that flow would linger in currentFlowsJson()'s visibility set forever.
            final Thread identity = threadIdentity();
            final String removedFlow;
            synchronized (mFlowStates) {
                removedFlow = mThreadFlow.remove(identity);
                mThreadNode.remove(identity);
            }
            // DIAGNOSTIC (temporary): the audit found NodeTerminatedEvent does NOT fire for
            // IEdge-interrupted threads, only for real dead ends and fork parents — this confirms
            // which of those this actually was.
            if (removedFlow != null) {
                mLogger.message("StudyMaster: thread " + System.identityHashCode(identity)
                        + " terminated while in flow '" + removedFlow + "'");
            }
            broadcastActiveFlows();

        } else if (event instanceof NodeExecutedEvent) {
            // DIAGNOSTIC (temporary, no state change yet): handleContinuation() fires this for the
            // OUTGOING node on every normal edge advance (NOT NodeTerminatedEvent, which the audit
            // found is reserved for dead ends / fork parents) — logging it should reveal whether a
            // "stuck" flow's thread is actually still progressing normally without ever reaching
            // NodeStartedEvent's "moved from X to Y" branch, which would point at a bug upstream of
            // this plugin's own bookkeeping rather than in it.
            final BasicNode executedNode = ((NodeExecutedEvent) event).getNode();
            final Thread identity = threadIdentity();
            final String trackedFlow;
            synchronized (mFlowStates) {
                trackedFlow = mThreadFlow.get(identity);
            }
            mLogger.message("StudyMaster: thread " + System.identityHashCode(identity)
                    + " executed node " + executedNode.getId() + " (" + executedNode.getName() + ")"
                    + ", currently tracked flow=" + trackedFlow);

        } else if (event instanceof SceneExecutedEvent) {
            final String sceneName = ((SceneExecutedEvent) event).getScene().getName();
            if (updateCurrentNodeVisit(visit -> {
                // A fresh play of a scene restarts its turn list — otherwise looping back through
                // the same scene would append forever instead of showing what was just said.
                visit.sceneName = sceneName;
                visit.sceneTurns = new ArrayList<>();
            })) {
                broadcastActiveFlows();
            }

        } else if (event instanceof TurnExecutedEvent) {
            final SceneTurn turn = ((TurnExecutedEvent) event).getTurn();
            final JSONObject turnJson = new JSONObject()
                    .put("speaker", turn.getSpeaker())
                    .put("text", turn.getCleanText());
            if (updateCurrentNodeVisit(visit -> visit.sceneTurns.add(turnJson))) {
                broadcastActiveFlows();
            }

        } else if (event instanceof VariableChangedEvent) {
            final Tuple<String, String> pair = ((VariableChangedEvent) event).getVarValue();
            if (mControls.containsKey(pair.getFirst())) {
                broadcastVariableChanged(pair.getFirst(), pair.getSecond());
            }
        }
    }

    /**
     * Process.getName() is frozen at construction to the id of whatever node the thread STARTED
     * at (verified: Process never calls setName(); handleContinuation()/handleInterruption()
     * mutate mCurrentNode in place without renaming) — stable for that Process's whole lifetime,
     * but not unique across time: a later run, or a loop back to the same start node, spawns a new
     * Process that freezes to the SAME name an earlier, by-then-dead Process already used. Keying
     * on the Process object itself sidesteps that entirely.
     * NodeStartedEvent/NodeTerminatedEvent fire with the Process itself as event source, and
     * ReactivePlayer.playScene() casts Thread.currentThread() to Process before firing
     * SceneExecutedEvent — so for those three, Thread.currentThread() IS the owning Process, and
     * this records it under its (possibly-reused) name in mNameToProcess for the case below.
     * TurnExecutedEvent/SceneDoneEvent instead fire from inside the PlayerWorker thread
     * ReactivePlayer spawns to run a scene's turns (RunTimePlayer.PlayerWorker); that thread's
     * name is built as "<processName>:<sceneName>@..." (see ReactivePlayer.playScene()), so the
     * prefix before the first ':' is looked up in mNameToProcess to recover whichever Process most
     * recently claimed that name — i.e. the live one, not a stale namesake.
     */
    private Thread threadIdentity() {
        final Thread t = Thread.currentThread();
        if (t instanceof Process) {
            synchronized (mFlowStates) {
                mNameToProcess.put(t.getName(), t);
            }
            return t;
        }
        final String name = t.getName();
        final int idx = name.indexOf(':');
        final String processName = idx >= 0 ? name.substring(0, idx) : name;
        synchronized (mFlowStates) {
            final Thread owner = mNameToProcess.get(processName);
            return owner != null ? owner : t;
        }
    }

    /**
     * An AliasNode (e.g. S18) is a flyweight reuse of another top-level SuperNode's subgraph (e.g.
     * S4) — its own children delegate to the canonical's actual shared node objects, so only the
     * entry AliasNode itself needs resolving here; anything started inside the aliased subgraph
     * already carries the canonical's real BasicNode instances. Mirrors
     * SceneFlowSnapshotBuilder's existing instanceof AliasNode / getCanonicalNode() handling.
     */
    private static BasicNode resolveAlias(BasicNode node) {
        if (node instanceof AliasNode) {
            final SuperNode canonical = ((AliasNode) node).getCanonicalNode();
            if (canonical != null) {
                return canonical;
            }
        }
        return node;
    }

    /** Walks up the node hierarchy to the direct child of the root SceneFlow that {@code node}
     * is nested under (itself, if it already is one) — the "top-level flow" a thread belongs to. */
    private static BasicNode topLevelFlowOf(BasicNode node) {
        BasicNode current = node;
        SuperNode parent = current.getParentNode();
        while (parent != null && !(parent instanceof SceneFlow)) {
            current = parent;
            parent = parent.getParentNode();
        }
        return current;
    }

    /**
     * Walks the whole SceneFlow tree once (at launch) collecting the id of every top-level
     * SuperNode targeted by an AliasNode anywhere in the project. getNodeAndSuperNodeList() only
     * returns direct children, so this recurses manually; an AliasNode's own children are the
     * canonical's shared objects (see resolveAlias()'s docs), so there's nothing new to find by
     * recursing into one — only its refId is collected.
     */
    private static void collectAliasCanonicalIds(SuperNode container, Set<String> out) {
        for (BasicNode child : container.getNodeAndSuperNodeList()) {
            if (child instanceof AliasNode) {
                final SuperNode canonical = ((AliasNode) child).getCanonicalNode();
                if (canonical != null) {
                    out.add(canonical.getId());
                }
            } else if (child instanceof SuperNode) {
                collectAliasCanonicalIds((SuperNode) child, out);
            }
        }
    }

    /**
     * SceneExecutedEvent/TurnExecutedEvent don't carry the node they belong to, only mThreadNode
     * (populated on NodeStartedEvent) says which node the firing thread is currently at. Falls
     * back to mLastVisit (whatever node most recently started, session-wide) if that per-thread
     * lookup comes up empty, rather than silently dropping the scene/turn.
     */
    private boolean updateCurrentNodeVisit(java.util.function.Consumer<NodeVisit> mutator) {
        final Thread identity = threadIdentity();
        synchronized (mFlowStates) {
            final String flowName = mThreadFlow.get(identity);
            final String nodeId = mThreadNode.get(identity);
            NodeVisit visit = null;
            if (flowName != null && nodeId != null) {
                final FlowState flowState = mFlowStates.get(flowName);
                visit = flowState != null ? flowState.visits.get(nodeId) : null;
            }
            if (visit == null) {
                visit = mLastVisit;
            }
            if (visit == null) {
                return false;
            }
            mutator.accept(visit);
            return true;
        }
    }

    private void broadcastActiveFlows() {
        sendToWizard(new JSONObject().put("type", "activeFlows").put("flows", currentFlowsJson()));
    }

    /**
     * A flow is worth showing the wizard only while it's still relevant: either some thread is
     * currently in it, or it's the single most recently fully-vacated flow (one step of trailing
     * context, e.g. "Name" stays visible right as "Repeat after me" starts). Anything vacated
     * before that — including the project's own root flow, which is just the first entry in that
     * same sequence — is no longer shown, though its history is kept internally in case the flow
     * is revisited later.
     */
    private JSONArray currentFlowsJson() {
        final JSONArray arr = new JSONArray();
        synchronized (mFlowStates) {
            final java.util.Set<String> visible = new java.util.HashSet<>();
            visible.addAll(mThreadFlow.values());
            if (mLastRetiredFlow != null) {
                visible.add(mLastRetiredFlow);
            }
            for (FlowState flowState : mFlowStates.values()) {
                if (visible.contains(flowState.flow)) {
                    arr.put(flowState.toJson());
                }
            }
        }
        return arr;
    }

    private void broadcastVariableChanged(String var, Object value) {
        sendToWizard(new JSONObject().put("type", "variableChanged").put("var", var).put("value", value));
    }

    // ---- Helpers --------------------------------------------------------------------------------

    private JSONObject buildSnapshot() {
        final JSONObject snapshot = new JSONObject();
        snapshot.put("type", "snapshot");
        // TODO: wire to actual pause/stop detection; hardcoded until the right runtime-state hook
        // is confirmed (RuntimeCommandService only exposes coarse running/stopped today).
        snapshot.put("runtimeStatus", "running");

        snapshot.put("flows", currentFlowsJson());

        final JSONArray controls = new JSONArray();
        for (ControlSpec spec : mControls.values()) {
            final JSONObject c = new JSONObject()
                    .put("var", spec.var)
                    .put("label", spec.label)
                    .put("kind", spec.kind)
                    .put("value", mProject.hasVariable(spec.var) ? mProject.getValueOf(spec.var).toString() : JSONObject.NULL);
            if (spec.options != null) c.put("options", spec.options);
            if (spec.min != null) c.put("min", spec.min);
            if (spec.max != null) c.put("max", spec.max);
            controls.put(c);
        }
        snapshot.put("controls", controls);

        synchronized (this) {
            snapshot.put("pendingRequest", mPendingRequest != null ? mPendingRequest : JSONObject.NULL);
        }
        return snapshot;
    }

    private void sendToWizard(JSONObject msg) {
        final WsContext socket;
        synchronized (this) {
            socket = mWizardSocket;
        }
        if (socket != null && socket.session.isOpen()) {
            socket.send(msg.toString());
        }
    }

    private void sendError(WsContext ctx, String code, String message) {
        if (ctx != null && ctx.session.isOpen()) {
            ctx.send(new JSONObject().put("type", "error").put("code", code).put("message", message).toString());
        }
    }

    private void loadControls() {
        final String controlsFileName = mConfig.getProperty(CONTROLS_FILE_PROP, CONTROLS_FILE_DEFAULT);
        final Path controlsPath = Path.of(mProject.getProjectPath(), controlsFileName);
        if (!Files.exists(controlsPath)) {
            mLogger.warning("StudyMaster: no controls manifest at " + controlsPath + " — no variables will be wizard-controllable");
            return;
        }
        try {
            final String raw = Files.readString(controlsPath);
            final JSONArray arr = new JSONArray(raw);
            for (int i = 0; i < arr.length(); i++) {
                final JSONObject o = arr.getJSONObject(i);
                final ControlSpec spec = new ControlSpec();
                spec.var = o.getString("var");
                spec.label = o.optString("label", spec.var);
                spec.kind = o.optString("kind", "string");
                if (o.has("options")) spec.options = o.getJSONArray("options");
                if (o.has("min")) spec.min = o.getDouble("min");
                if (o.has("max")) spec.max = o.getDouble("max");
                mControls.put(spec.var, spec);
            }
        } catch (Exception e) {
            mLogger.failure("StudyMaster: failed to parse " + controlsPath + ": " + e);
        }
    }

    private static final class FlowState {
        final String flow;
        // Keyed by node id, insertion-ordered: the running history of nodes this flow has visited.
        final Map<String, NodeVisit> visits = new LinkedHashMap<>();

        FlowState(String flow) {
            this.flow = flow;
        }

        JSONObject toJson() {
            final JSONArray nodes = new JSONArray();
            for (NodeVisit visit : visits.values()) {
                nodes.put(visit.toJson());
            }
            return new JSONObject().put("flow", flow).put("nodes", nodes);
        }
    }

    private static final class NodeVisit {
        JSONObject node; // {id,name,comment}
        String sceneName; // last scene played at this node, nullable — kept even after it finishes
        List<JSONObject> sceneTurns = new ArrayList<>(); // {speaker,text}, accumulated for that play

        JSONObject toJson() {
            final JSONArray turns = new JSONArray();
            for (JSONObject t : sceneTurns) {
                turns.put(t);
            }
            return new JSONObject()
                    .put("id", node.optString("id"))
                    .put("name", node.optString("name"))
                    .put("comment", node.optString("comment"))
                    .put("scene", sceneName != null ? sceneName : JSONObject.NULL)
                    .put("turns", turns);
        }
    }

    private static final class ControlSpec {
        String var;
        String label;
        String kind;
        JSONArray options;
        Double min;
        Double max;
    }
}
