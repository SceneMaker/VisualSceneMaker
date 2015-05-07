package de.dfki.vsm.runtime;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.runtime.error.RunTimeException;
import de.dfki.vsm.runtime.event.AbortEvent;
import de.dfki.vsm.runtime.value.BooleanValue;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.log.LOGDefaultLogger;

public class EventObserver {
    private Interpreter mInterpreter;

    public EventObserver(Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    public void update() {
        Object[] array = mInterpreter.getConfiguration().getOrderedStates();

        for (Object obj : array) {
            Configuration.State state = (Configuration.State) obj;

            if (state.getNode() instanceof SuperNode) {
                for (IEdge iedge : state.getNode().getIEdgeList()) {
                    try {
                        if (((BooleanValue) mInterpreter.getEvaluator().evaluate(iedge.getCondition(),
                                state.getThread().getEnvironment())).getValue()) {
                            state.getThread().requestInterruption(iedge);

                            break;
                        }
                    } catch (RunTimeException e) {
                        LOGDefaultLogger.getInstance().warning("detecting abort in observer");
                        EventCaster.getInstance().convey(new AbortEvent(this, e));
                        mInterpreter.stop();
                        LOGDefaultLogger.getInstance().warning("returnning from observer after runtimeexception");

                        return;
                    } catch (ClassCastException e) {
                        LOGDefaultLogger.getInstance().warning("detecting abort in observer");

                        java.lang.String errorMsg = "An error occured while executing thread "
                                                    + Process.currentThread().toString() + " : " + "The condition '"
                                                    + iedge.getCondition().getConcreteSyntax()
                                                    + "' of the interruptive edge from node '" + iedge.getSource()
                                                    + "' to node '" + iedge.getTarget()
                                                    + "' could not be evaluated to a boolean value.";
                        RunTimeException exception = new RunTimeException(this, errorMsg);

                        EventCaster.getInstance().convey(new AbortEvent(this, exception));
                        mInterpreter.stop();

                        return;
                    }
                }
            }
        }
    }
}
