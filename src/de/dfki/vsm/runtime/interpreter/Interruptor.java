package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.runtime.exception.InterpretException;
import de.dfki.vsm.runtime.event.AbortionEvent;
import de.dfki.vsm.runtime.interpreter.Configuration.State;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.BooleanValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;

public final class Interruptor {

    // The logger instance
    private final LOGDefaultLogger mLogger = 
            LOGDefaultLogger.getInstance();
    // The interpreter instance
    private final Interpreter mInterpreter;

    // Create the interruptor
    public Interruptor(final Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    // Update the interruptor
    public final void update() {
        // Get the runtime evaluator
        final Evaluator evaluator = mInterpreter.getEvaluator();
        // Get the runtime configuration
        final Configuration configuration = mInterpreter.getConfiguration();
        // Get the ordered list of states
        final Object[] states = configuration.getOrderedStates();
        // Iterate over the list of states
        for (final Object object : states) {
            final State state = (State) object;
            // Check the node of the state            
            //if (state.getNode() instanceof SuperNode) {
            // Iterate over the list of interruptive edges
            for (final IEdge iedge : state.getNode().getIEdgeList()) {
                try {
                    // Get the condition of the edge
                    final Condition condition = iedge.getCondition();
                    // Get the current environment
                    final Environment environment = state.getThread().getEnvironment();
                    // Evauate the condition then
                    final AbstractValue value = evaluator.evaluate(condition, environment);
                    // Check the evaluation result
                    if (((BooleanValue) value).getValue()) {
                        // Request the interruption of the process
                        state.getThread().requestInterruption(iedge);
                        // Stop update loop if an edge has been found
                        break;
                    }
                } catch (final InterpretException exc) {
                    mLogger.warning("detecting abort in observer");
                    EventDispatcher.getInstance().convey(new AbortionEvent(this, exc));
                    mInterpreter.abort();
                    mLogger.warning("returnning from observer after runtimeexception");

                    return;
                } catch (final ClassCastException exc) {
                    mLogger.warning("detecting abort in observer");

                    String errorMsg = "An error occured while executing thread "
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
