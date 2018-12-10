package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayDialogAction;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.TimeoutQuery;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;

import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

/**
 * @author Gregor Mehlmann
 */
public class TimeoutManager {

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final HashMap<TimeoutQuery, TPLTuple<Boolean, TimerTask>> mTimeoutCondList = new HashMap<TimeoutQuery, TPLTuple<Boolean, TimerTask>>();
    private final Timer mTimer = new Timer("Timeout-Manager-Timer");
    private Interpreter mInterpreter;

    public TimeoutManager(Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    public boolean contains(final TimeoutQuery cond) {
        return mTimeoutCondList.containsKey(cond);
    }

    public synchronized void abort() {
        mTimer.cancel();
    }

    public void clear() {
        for (TPLTuple<Boolean, TimerTask> pair : mTimeoutCondList.values()) {
            pair.setFirst(false);
            pair.getSecond().cancel();
        }

        mTimeoutCondList.clear();
        mLogger.message("Clearing timeout manager");
    }

    public boolean expired(final TimeoutQuery cond) {
        return mTimeoutCondList.get(cond).getFirst();
    }

    public void remove(final TimeoutQuery cond) {

        // mTimeoutCondList.get(cond).
        mTimeoutCondList.get(cond).getSecond().cancel();
        mTimeoutCondList.remove(cond);
        mLogger.message("removing " + cond.getConcreteSyntax() + " ");
    }

    public void start(final TimeoutQuery cond, int timeout) {
        if (contains(cond)) {
            mLogger.message("Already contained " + cond.getConcreteSyntax() + " -> restart");
            remove(cond);
        }

        final TimerTask task = new TimerTask() {
            @Override
            public void run() {
                mInterpreter.lock();

                if (contains(cond)) {
                    mTimeoutCondList.get(cond).setFirst(true);
                    mLogger.message("Setting true " + cond.getConcreteSyntax());
                }

                // if (mEventObserver != null) {
                mInterpreter.getEventObserver().update();

                // }
                cancel();
                mInterpreter.unlock();
            }
        };

        mTimeoutCondList.put(cond, new TPLTuple<Boolean, TimerTask>(false, task));
        mTimer.schedule(task, timeout);
    }

    ///////////////////////////////////////////////////////////////////////////
    public void startTimeoutHandler(VariableDefinition def, Environment env) throws InterpreterError {
        startTimeoutHandler(def.getExp(), env);
    }

    public void startTimeoutHandler(Command cmd, Environment env) throws InterpreterError {
        if (cmd instanceof PlayScenesActivity) {
            startTimeoutHandler(((PlayScenesActivity) cmd).getArgument(), env);

            for (Expression arg : ((PlayScenesActivity) cmd).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (cmd instanceof PlayDialogAction) {
            startTimeoutHandler(((PlayDialogAction) cmd).getArg(), env);

            for (Expression arg : ((PlayDialogAction) cmd).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (cmd instanceof UnblockSceneGroup) {
            startTimeoutHandler(((UnblockSceneGroup) cmd).getArg(), env);
        } else if (cmd instanceof Assignment) {
            startTimeoutHandler(((Assignment) cmd).getLeftExpression(), env);
            startTimeoutHandler(((Assignment) cmd).getInitExpression(), env);
        } else if (cmd instanceof Expression) {
            startTimeoutHandler((Expression) cmd, env);
        }
    }

    public void startTimeoutHandler(Expression exp, Environment env) throws InterpreterError {
        if (exp instanceof ArrayExpression) {
            for (Expression arg : ((ArrayExpression) exp).getExpList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (exp instanceof StructExpression) {
            for (Assignment arg : ((StructExpression) exp).getExpList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (exp instanceof BinaryExpression) {
            startTimeoutHandler(((BinaryExpression) exp).getLeftExp(), env);
            startTimeoutHandler(((BinaryExpression) exp).getRightExp(), env);
        } else if (exp instanceof UnaryExpression) {
            startTimeoutHandler(((UnaryExpression) exp).getExp(), env);
        } else if (exp instanceof TernaryExpression) {
            startTimeoutHandler(((TernaryExpression) exp).getCondition(), env);
            startTimeoutHandler(((TernaryExpression) exp).getThenExp(), env);
            startTimeoutHandler(((TernaryExpression) exp).getElseExp(), env);
        } else if (exp instanceof ArrayVariable) {
            startTimeoutHandler(((ArrayVariable) exp).getExpression(), env);
        } 
//        else if (exp instanceof BinaryCond) {
//            startTimeoutHandler(((BinaryCond) exp).getLeftCond(), env);
//            startTimeoutHandler(((BinaryCond) exp).getRightCond(), env);
//        } else if (exp instanceof UnaryCond) {
//            startTimeoutHandler(((UnaryCond) exp).getCondition(), env);
//        } else if (exp instanceof ComparisionCond) {
//            startTimeoutHandler(((ComparisionCond) exp).getLeftExp(), env);
//            startTimeoutHandler(((ComparisionCond) exp).getRightExp(), env);
//        } else if (exp instanceof EmptyCond) {
//            startTimeoutHandler(((EmptyCond) exp).getExp(), env);
//        } 
        else if (exp instanceof TimeoutQuery) {

            /**
             * START TIMEOUT CONDITION TIMER
             */
            startTimeoutHandler(((TimeoutQuery) exp).getExpression(), env);

            AbstractValue value = mInterpreter.getEvaluator().evaluate(((TimeoutQuery) exp).getExpression(), env);

            if (value.getType() == AbstractValue.Type.INT) {
                start((TimeoutQuery) exp, ((IntValue) value).getValue());
            } else if (value.getType() == AbstractValue.Type.STRING) {
                start((TimeoutQuery) exp, Integer.parseInt(((StringValue) value).getValue()));
            } else {
                throw new InterpreterError(this, "timeout argument not integer");
            }
        } 
//        else if (exp instanceof DefaultCond) {
//            startTimeoutHandler(((DefaultCond) exp).getCondition(), env);
//        } 
        else if (exp instanceof CallingExpression) {
            for (Expression arg : ((CallingExpression) exp).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else {
            return;
        }
    }
}
