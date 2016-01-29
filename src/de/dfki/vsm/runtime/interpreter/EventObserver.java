package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.runtime.exceptions.InterpretException;
import de.dfki.vsm.runtime.events.AbortionEvent;
import de.dfki.vsm.runtime.values.BooleanValue;
import de.dfki.vsm.util.evt.EventDispatcher;
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

            //if (state.getNode() instanceof SuperNode) {
                for (IEdge iedge : state.getNode().getIEdgeList()) {
                    try {
                        if (((BooleanValue) mInterpreter.getEvaluator().evaluate(iedge.getCondition(),
                                state.getThread().getEnvironment())).getValue()) {
                            state.getThread().requestInterruption(iedge);

                            break;
                        }
                    } catch (InterpretException e) {
                        LOGDefaultLogger.getInstance().warning("detecting abort in observer");
                        EventDispatcher.getInstance().convey(new AbortionEvent(this, e));
                        mInterpreter.abort();
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
                        InterpretException exception = new InterpretException(this, errorMsg);

                        EventDispatcher.getInstance().convey(new AbortionEvent(this, exception));
                        mInterpreter.abort();

                        return;
                    }
                }
            //}
        }
    }
}
