package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Assignment;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.PlayDialogueAct;
import de.dfki.vsm.model.sceneflow.command.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.command.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.command.expression.BinaryExp;
import de.dfki.vsm.model.sceneflow.command.expression.ConditionalExp;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.UnaryExp;
import de.dfki.vsm.model.sceneflow.command.expression.UsrCmd;
import de.dfki.vsm.model.sceneflow.command.expression.condition.EmptyCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.List;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Struct;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.ArrVarExp;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.BinaryCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.ComparisionCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.DefaultCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.UnaryCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.temporal.TimeoutCond;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.exceptions.InterpretException;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.IntValue;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Timer;
import java.util.TimerTask;

/**
 * @author Not me
 */
public class TimeoutManager {
    private final LOGDefaultLogger                                   mLogger          = LOGDefaultLogger.getInstance();
    private final HashMap<TimeoutCond, TPLTuple<Boolean, TimerTask>> mTimeoutCondList = new HashMap<TimeoutCond,
                                                                                            TPLTuple<Boolean,
                                                                                                TimerTask>>();
    private final Timer mTimer = new Timer("Timeout-Manager-Timer");
    private Interpreter mInterpreter;

    public TimeoutManager(Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    public boolean contains(final TimeoutCond cond) {
        return mTimeoutCondList.containsKey(cond);
    }

    public synchronized void cancel() {
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

    public boolean expired(final TimeoutCond cond) {
        return mTimeoutCondList.get(cond).getFirst();
    }

    public void remove(final TimeoutCond cond) {

        // mTimeoutCondList.get(cond).
        mTimeoutCondList.get(cond).getSecond().cancel();
        mTimeoutCondList.remove(cond);
        mLogger.message("removing " + cond.getConcreteSyntax() + " ");
    }

    public void start(final TimeoutCond cond, int timeout) {
        if (contains(cond)) {
            mLogger.message("Already contained " + cond.getConcreteSyntax() + " -> restart");
            remove(cond);
        }

        final TimerTask task = new TimerTask() {
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
    public void startTimeoutHandler(VarDef def, Environment env) throws InterpretException {
        startTimeoutHandler(def.getExp(), env);
    }

    public void startTimeoutHandler(Command cmd, Environment env) throws InterpretException {
        if (cmd instanceof PlaySceneGroup) {
            startTimeoutHandler(((PlaySceneGroup) cmd).getArg(), env);

            for (Expression arg : ((PlaySceneGroup) cmd).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (cmd instanceof PlayDialogueAct) {
            startTimeoutHandler(((PlayDialogueAct) cmd).getArg(), env);

            for (Expression arg : ((PlayDialogueAct) cmd).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (cmd instanceof UnblockSceneGroup) {
            startTimeoutHandler(((UnblockSceneGroup) cmd).getArg(), env);
        } else if (cmd instanceof Assignment) {
            startTimeoutHandler(((Assignment) cmd).getLExp(), env);
            startTimeoutHandler(((Assignment) cmd).getExp(), env);
        } else if (cmd instanceof Expression) {
            startTimeoutHandler((Expression) cmd, env);
        }
    }

    public void startTimeoutHandler(Expression exp, Environment env) throws InterpretException {
        if (exp instanceof List) {
            for (Expression arg : ((List) exp).getExpList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (exp instanceof Struct) {
            for (Assignment arg : ((Struct) exp).getExpList()) {
                startTimeoutHandler(arg, env);
            }
        } else if (exp instanceof BinaryExp) {
            startTimeoutHandler(((BinaryExp) exp).getLeftExp(), env);
            startTimeoutHandler(((BinaryExp) exp).getRightExp(), env);
        } else if (exp instanceof UnaryExp) {
            startTimeoutHandler(((UnaryExp) exp).getExp(), env);
        } else if (exp instanceof ConditionalExp) {
            startTimeoutHandler(((ConditionalExp) exp).getCondition(), env);
            startTimeoutHandler(((ConditionalExp) exp).getThenExp(), env);
            startTimeoutHandler(((ConditionalExp) exp).getElseExp(), env);
        } else if (exp instanceof ArrVarExp) {
            startTimeoutHandler(((ArrVarExp) exp).getExp(), env);
        } else if (exp instanceof BinaryCond) {
            startTimeoutHandler(((BinaryCond) exp).getLeftCond(), env);
            startTimeoutHandler(((BinaryCond) exp).getRightCond(), env);
        } else if (exp instanceof UnaryCond) {
            startTimeoutHandler(((UnaryCond) exp).getCondition(), env);
        } else if (exp instanceof ComparisionCond) {
            startTimeoutHandler(((ComparisionCond) exp).getLeftExp(), env);
            startTimeoutHandler(((ComparisionCond) exp).getRightExp(), env);
        } else if (exp instanceof EmptyCond) {
            startTimeoutHandler(((EmptyCond) exp).getExp(), env);
        } else if (exp instanceof TimeoutCond) {

            /**
             * START TIMEOUT CONDITION TIMER
             */
            startTimeoutHandler(((TimeoutCond) exp).getTimeout(), env);

            AbstractValue value = mInterpreter.getEvaluator().evaluate(((TimeoutCond) exp).getTimeout(), env);

            if (value.getType() == AbstractValue.Type.INT) {
                start((TimeoutCond) exp, ((IntValue) value).getValue());
            } else if (value.getType() == AbstractValue.Type.STRING) {
                start((TimeoutCond) exp, Integer.parseInt(((StringValue) value).getValue()));
            } else {
                throw new InterpretException(this, "timeout argument not integer");
            }
        } else if (exp instanceof DefaultCond) {
            startTimeoutHandler(((DefaultCond) exp).getCondition(), env);
        } else if (exp instanceof UsrCmd) {
            for (Expression arg : ((UsrCmd) exp).getArgList()) {
                startTimeoutHandler(arg, env);
            }
        } else {
            return;
        }
    }
}
