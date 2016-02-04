package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.Assignment;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.HistoryClear;
import de.dfki.vsm.model.sceneflow.command.HistoryDeepClear;
import de.dfki.vsm.model.sceneflow.command.HistorySetDepth;
import de.dfki.vsm.model.sceneflow.command.PlayDialogueAct;
import de.dfki.vsm.model.sceneflow.command.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.command.UnblockAllSceneGroups;
import de.dfki.vsm.model.sceneflow.command.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.command.expression.BinaryExp;
import de.dfki.vsm.model.sceneflow.command.expression.ConditionalExp;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.HistoryRunTimeOf;
import de.dfki.vsm.model.sceneflow.command.expression.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.command.expression.UnaryExp;
import de.dfki.vsm.model.sceneflow.command.expression.UsrCmd;
import de.dfki.vsm.model.sceneflow.command.expression.ValueOf;
import de.dfki.vsm.model.sceneflow.command.expression.condition.ContainsCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.EmptyCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Float;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Int;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.List;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Object;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.String;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Struct;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.ArrVarExp;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.LExpression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.MemVarExp;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.VarExp;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.BinaryCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.ComparisionCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.DefaultCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.HistoryContainsState;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.InStateCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.PrologCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.UnaryCond;
import de.dfki.vsm.model.sceneflow.command.expression.condition.temporal.TimeoutCond;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.exceptions.InterpretException;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.BooleanValue;
import de.dfki.vsm.runtime.values.DoubleValue;
import de.dfki.vsm.runtime.values.FloatValue;
import de.dfki.vsm.runtime.values.IntValue;
import de.dfki.vsm.runtime.values.ListValue;
import de.dfki.vsm.runtime.values.LongValue;
import de.dfki.vsm.runtime.values.ObjectValue;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.runtime.values.StructValue;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Random;
import java.util.Vector;

/**
 * @author Not me
 */
public class Evaluator {

    // The singelton logger instance 
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The parent interpreter object
    private final Interpreter mInterpreter;

    // Construct the evaluator with the interpreter
    public Evaluator(final Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    // Execute a command
    public final void execute(final Command cmd, final Environment env) throws InterpretException {

        // Execute a scene group playback command
        if (cmd instanceof PlaySceneGroup) {
            AbstractValue value = evaluate(((PlaySceneGroup) cmd).getArg(), env);
            LinkedList<AbstractValue> valueList = evaluateExpList(((PlaySceneGroup) cmd).getArgList(), env);

            if (value.getType() == AbstractValue.Type.STRING) {
                mInterpreter.unlock();
                mInterpreter.getScenePlayer().play(((StringValue) value).getValue(), valueList);
                mInterpreter.lock();
            } else {
                java.lang.String errorMsg = "An error occured while executing thread "
                        + Process.currentThread().toString() + " : "
                        + "The scene group argument of the playback command '"
                        + cmd.getConcreteSyntax() + " was evaluated to '"
                        + value.getConcreteSyntax() + "' which is not a string constant";

                throw new InterpretException(cmd, errorMsg);
            }
        } ////////////////////////////////////////////////////////////////////
        // PLAY DIALOGUE ACT
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof PlayDialogueAct) {

            // Try To Evaluate The Name Expression
            final AbstractValue name = evaluate(((PlayDialogueAct) cmd).getArg(), env);

            // Try To Evaluate The Feature List
            LinkedList<AbstractValue> valueList = evaluateExpList(((PlayDialogueAct) cmd).getArgList(), env);

            // Check The Type of The Name Expression
            if (name.getType() == AbstractValue.Type.STRING) {

                // Unlock The Interpreter Lock Now
                mInterpreter.unlock();

                // Execute The Dialogue Act Player
                mInterpreter.getDialoguePlayer().play(((StringValue) name).getValue(), valueList);

                // Relock The Interpreter Lock Now
                mInterpreter.lock();
            } else {
                java.lang.String errorMsg = "An error occured while executing thread "
                        + Process.currentThread().toString() + " : "
                        + "The dialogue act argument of the playback command '"
                        + cmd.getConcreteSyntax() + " was evaluated to '"
                        + name.getConcreteSyntax() + "' which is not a string constant";

                throw new InterpretException(cmd, errorMsg);
            }
        } ////////////////////////////////////////////////////////////////////
        // UnblockSceneGroup
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof UnblockSceneGroup) {
            throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } //
        ////////////////////////////////////////////////////////////////////
        // UnblockAllSceneGroups
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof UnblockAllSceneGroups) {
            throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } //
        ////////////////////////////////////////////////////////////////////
        // Assignment
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof Assignment) {
            LExpression lexp = ((Assignment) cmd).getLExp();
            Expression rexp = ((Assignment) cmd).getExp();

            ////////////////////////////////////////////////////////////////////
            // Simple Variable Assignment
            ////////////////////////////////////////////////////////////////////
            if (lexp instanceof VarExp) {
                env.write(((VarExp) lexp).getName(), evaluate(rexp, env));
            } //
            ////////////////////////////////////////////////////////////////////
            // Array Field Assignment
            ////////////////////////////////////////////////////////////////////
            else if (lexp instanceof ArrVarExp) {
                AbstractValue fieldValue = evaluate(((ArrVarExp) lexp).getExp(), env);

                if (fieldValue.getType() == AbstractValue.Type.INT) {
                    env.write(((ArrVarExp) lexp).getName(), ((IntValue) fieldValue).getValue().intValue(),
                            evaluate(rexp, env));
                } else {
                    throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // Member Variable Assignment
            ////////////////////////////////////////////////////////////////////
            else if (lexp instanceof MemVarExp) {
                env.write(((MemVarExp) lexp).getName(), ((MemVarExp) lexp).getMemberName(), evaluate(rexp, env));
            } //
            ////////////////////////////////////////////////////////////////////
            // Assignment
            ////////////////////////////////////////////////////////////////////
            else {
                throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // HistoryClear
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof HistoryClear) {
            mInterpreter.getSystemHistory().erase(((HistoryClear) cmd).getState());
        } //
        ////////////////////////////////////////////////////////////////////
        // HistoryDeepClear
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof HistoryDeepClear) {
            mInterpreter.getSystemHistory().deepErase(((HistoryDeepClear) cmd).getState());
        } //
        ////////////////////////////////////////////////////////////////////
        // HistorySetDepth
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof HistorySetDepth) {
            mInterpreter.getSystemHistory().setDepth(((HistorySetDepth) cmd).getState(),
                    ((HistorySetDepth) cmd).getDepth());
        } //
        ////////////////////////////////////////////////////////////////////
        // EVALUATE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else {
            try {
                evaluate((Expression) cmd, env);
            } catch (Exception e) {
                throw new InterpretException(env, "Runtime Error: '" + cmd.toString() + "' cannot be evaluated.");
            }
        }
    }

    /**
     * Declare a variable
     *
     * @param def
     * @param env
     * @throws InterpretException
     */
    public void declare(VarDef def, Environment env) throws InterpretException {
        env.create(def.getName(), evaluate(def.getExp(), env));
    }

    /**
     *
     * Evaluate an expression
     *
     */
    public AbstractValue evaluate(Expression exp, Environment env) throws InterpretException {

        ////////////////////////////////////////////////////////////////////
        // CONSTANT EXPRESSION
        ////////////////////////////////////////////////////////////////////
        if (exp instanceof Bool) {
            return new BooleanValue(((Bool) exp).getValue());
        } else if (exp instanceof Int) {
            return new IntValue(((Int) exp).getValue());
        } else if (exp instanceof Float) {
            return new FloatValue(((Float) exp).getValue());
        } else if (exp instanceof String) {
            return new StringValue(((String) exp).getValue());
        } else if (exp instanceof Object) {
            return new ObjectValue(((Object) exp).getValue());
        } else if (exp instanceof List) {
            return new ListValue(evaluateExpList(((List) exp).getExpList(), env));
        } else if (exp instanceof Struct) {
            return new StructValue(evaluateAsgList(((Struct) exp).getExpList(), env));
        } //
        ////////////////////////////////////////////////////////////////////
        // BINARY EXPRESSIONS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof BinaryExp) {
            AbstractValue left = evaluate(((BinaryExp) exp).getLeftExp(), env);
            AbstractValue right = evaluate(((BinaryExp) exp).getRightExp(), env);
            BinaryExp.Operator operator = ((BinaryExp) exp).getOperator();

            ////////////////////////////////////////////////////////////////////
            // OPERATOR +
            ////////////////////////////////////////////////////////////////////
            if (operator == BinaryExp.Operator.Add) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() + ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new FloatValue(((FloatValue) left).floatValue() + ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new LongValue(((LongValue) left).longValue() + ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new DoubleValue(((DoubleValue) left).doubleValue() + ((DoubleValue) right).doubleValue());
                } else if ((left instanceof StringValue) && (right instanceof StringValue)) {
                    return new StringValue(((StringValue) left).getValue() + ((StringValue) right).getValue());
                } else if ((left instanceof StringValue) && (right instanceof IntValue)) {
                    return new StringValue(((StringValue) left).getValue() + ((IntValue) right).intValue());
                } else if ((left instanceof StringValue) && (right instanceof FloatValue)) {
                    return new StringValue(((StringValue) left).getValue() + ((FloatValue) right).floatValue());
                } else if ((left instanceof StringValue) && (right instanceof BooleanValue)) {
                    return new StringValue(((StringValue) left).getValue() + ((BooleanValue) right).booleanValue());
                } else if ((left instanceof StringValue) && (right instanceof LongValue)) {
                    return new StringValue(((StringValue) left).getValue() + ((LongValue) right).longValue());
                } else if ((left instanceof IntValue) && (right instanceof StringValue)) {
                    return new StringValue(((IntValue) left).intValue() + ((StringValue) right).getValue());
                } else if ((left instanceof FloatValue) && (right instanceof StringValue)) {
                    return new StringValue(((FloatValue) left).floatValue() + ((StringValue) right).getValue());
                } else if ((left instanceof BooleanValue) && (right instanceof StringValue)) {
                    return new StringValue(((BooleanValue) left).booleanValue() + ((StringValue) right).getValue());
                } else if ((left instanceof LongValue) && (right instanceof StringValue)) {
                    return new StringValue(((LongValue) left).longValue() + ((StringValue) right).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // OPERATOR -
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.Sub) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() - ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new FloatValue(((FloatValue) left).floatValue() - ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new LongValue(((LongValue) left).longValue() - ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new DoubleValue(((DoubleValue) left).doubleValue() - ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // OPERATOR *
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.Mul) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() * ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new FloatValue(((FloatValue) left).floatValue() * ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new LongValue(((LongValue) left).longValue() * ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new DoubleValue(((DoubleValue) left).doubleValue() * ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // OPERATOR /
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.Div) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() / ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new FloatValue(((FloatValue) left).floatValue() / ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new LongValue(((LongValue) left).longValue() / ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new DoubleValue(((DoubleValue) left).doubleValue() / ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } ////////////////////////////////////////////////////////////////////
            // OPERATOR AddFirst
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.AddFirst) {
                if (left instanceof ListValue) {

                    // Convert The Left Expression
                    final ListValue listValue = (ListValue) left;

                    // Get The Java List From Value
                    final LinkedList<AbstractValue> list = listValue.getValueList();

                    // Add The Abstract Value Here
                    list.addFirst(right);

                    //
                    return left;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // OPERATOR AddLast
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.AddLast) {
                if (left instanceof ListValue) {
                    ((ListValue) left).getValueList().addLast(right);

                    return left;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // OPERATOR Get
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.Get) {
                if ((left instanceof ListValue) && (right instanceof IntValue)) {
                    return ((ListValue) left).getValueList().get(((IntValue) right).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // AddFirst ( <list> , <exp> )
            ////////////////////////////////////////////////////////////////////
            else if (operator == BinaryExp.Operator.Remove) {
                if ((left instanceof ListValue) && (right instanceof IntValue)) {
                    return ((ListValue) left).getValueList().remove(((IntValue) right).getValue().intValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // Unknown binary operator
            ////////////////////////////////////////////////////////////////////
            else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // UNARY EXPRESSIONS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof UnaryExp) {
            AbstractValue value = evaluate(((UnaryExp) exp).getExp(), env);
            UnaryExp.Operator operator = ((UnaryExp) exp).getOperator();

            ////////////////////////////////////////////////////////////////////
            // Size
            ////////////////////////////////////////////////////////////////////
            if (operator == UnaryExp.Operator.Size) {
                if (value instanceof ListValue) {
                    return new IntValue(((ListValue) value).getValueList().size());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.Clear) {
                if (value instanceof ListValue) {
                    ((ListValue) value).getValueList().clear();

                    return value;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.First) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().getFirst();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.Last) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().getLast();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.RemoveFirst) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().removeFirst();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.RemoveLast) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().removeLast();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.Random) {
                if (value instanceof IntValue) {
                    int random = new Random().nextInt(((IntValue) value).getValue().intValue());

                    // System.err.println("Generating random number " + random);
                    return new IntValue(random);
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExp.Operator.Neg) {
                if (value instanceof IntValue) {
                    return new IntValue(-((IntValue) value).getValue());
                } else if (value instanceof FloatValue) {
                    return new FloatValue(-((FloatValue) value).floatValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // CONDITIONAL EXPRESSIONzuu
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof ConditionalExp) {
            AbstractValue condition = evaluate(((ConditionalExp) exp).getCondition(), env);
            AbstractValue thenValue = evaluate(((ConditionalExp) exp).getThenExp(), env);
            AbstractValue elseValue = evaluate(((ConditionalExp) exp).getElseExp(), env);

            if (condition instanceof BooleanValue) {
                if (((BooleanValue) condition).getValue()) {
                    return thenValue;
                } else {
                    return elseValue;
                }
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // SIMPLE VARIABLE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof VarExp) {
            return env.read(((VarExp) exp).getName());
        } //
        ////////////////////////////////////////////////////////////////////
        // ARRAY VARIABLE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof ArrVarExp) {
            AbstractValue index = evaluate(((ArrVarExp) exp).getExp(), env);

            if (index.getType() == AbstractValue.Type.INT) {
                return env.read(((ArrVarExp) exp).getName(), ((IntValue) index).getValue());
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // MEMBER VARIABLE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof MemVarExp) {
            return env.read(((MemVarExp) exp).getName(), ((MemVarExp) exp).getMemberName());
        } //
        ////////////////////////////////////////////////////////////////////
        // BINARY CONDITIONS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof BinaryCond) {

            // System.err.println("Condition= " + exp.getAbstractSyntax());
            AbstractValue left = evaluate(((BinaryCond) exp).getLeftCond(), env);
            AbstractValue right = evaluate(((BinaryCond) exp).getRightCond(), env);
            BinaryCond.Operator operator = ((BinaryCond) exp).getOperator();

            if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                if (operator == BinaryCond.Operator.And) {
                    return new BooleanValue(((BooleanValue) left).getValue() && ((BooleanValue) right).getValue());
                } else if (operator == BinaryCond.Operator.Or) {
                    return new BooleanValue(((BooleanValue) left).getValue() || ((BooleanValue) right).getValue());
                } else {
                    throw new InterpretException(exp,
                            "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
                }
            } else {
                throw new InterpretException(exp,
                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
            }
        } else if (exp instanceof UnaryCond) {

            /**
             * UNARY CONDITIONS
             */
            AbstractValue value = evaluate(((UnaryCond) exp).getCondition(), env);
            UnaryCond.Operator operator = ((UnaryCond) exp).getOperator();

            if (value instanceof BooleanValue) {
                if (operator == UnaryCond.Operator.Not) {
                    return new BooleanValue(!((BooleanValue) value).getValue());
                } else {
                    throw new InterpretException(exp,
                            "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
                }
            } else {
                throw new InterpretException(exp,
                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
            }
        } else if (exp instanceof ComparisionCond) {

            /**
             * COMPARISION CONDITIONS
             */
            AbstractValue left = evaluate(((ComparisionCond) exp).getLeftExp(), env);
            AbstractValue right = evaluate(((ComparisionCond) exp).getRightExp(), env);
            ComparisionCond.Operator operator = ((ComparisionCond) exp).getOperator();

            if (operator == ComparisionCond.Operator.Eq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(left.equalsValue(right));
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
                }
            }

            if (operator == ComparisionCond.Operator.Neq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(!(left.equalsValue(right)));
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
                }
            } else if (operator == ComparisionCond.Operator.Ge) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).getValue().intValue()
                            >= ((IntValue) right).getValue().intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
                            >= ((FloatValue) right).getValue().floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).getValue().longValue()
                            >= ((LongValue) right).getValue().longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
                            >= ((DoubleValue) right).getValue().doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == ComparisionCond.Operator.Gt) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).getValue().intValue()
                            > ((IntValue) right).getValue().intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
                            > ((FloatValue) right).getValue().floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).getValue().longValue()
                            > ((LongValue) right).getValue().longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
                            > ((DoubleValue) right).getValue().doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == ComparisionCond.Operator.Le) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).getValue().intValue()
                            <= ((IntValue) right).getValue().intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
                            <= ((FloatValue) right).getValue().floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).getValue().longValue()
                            <= ((LongValue) right).getValue().longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
                            <= ((DoubleValue) right).getValue().doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == ComparisionCond.Operator.Lt) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).getValue().intValue()
                            < ((IntValue) right).getValue().intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
                            < ((FloatValue) right).getValue().floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).getValue().longValue()
                            < ((LongValue) right).getValue().longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
                            < ((DoubleValue) right).getValue().doubleValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // CONTAINS LIST CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof ContainsCond) {
            AbstractValue listValue = evaluate(((ContainsCond) exp).getListExp(), env);
            AbstractValue elemValue = evaluate(((ContainsCond) exp).getElementExp(), env);

            if (listValue.getType() == AbstractValue.Type.LIST) {
                for (AbstractValue value : ((ListValue) listValue).getValueList()) {
                    if (value.getType() == elemValue.getType()) {
                        if (value.equalsValue(elemValue)) {
                            return new BooleanValue(true);
                        }
                    }
                }

                return new BooleanValue(false);
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // EMPTY LIST CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof EmptyCond) {
            AbstractValue value = evaluate(((EmptyCond) exp).getExp(), env);

            if (value.getType() == AbstractValue.Type.LIST) {
                return new BooleanValue(((ListValue) value).isEmpty());
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // TIMEOUT CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof TimeoutCond) {
            if (mInterpreter.getTimeoutManager().contains((TimeoutCond) exp)) {
                if (mInterpreter.getTimeoutManager().expired((TimeoutCond) exp)) {
                    mInterpreter.getTimeoutManager().remove((TimeoutCond) exp);

                    return new BooleanValue(true);
                } else {
                    return new BooleanValue(false);
                }
            } else {
                return new BooleanValue(false);
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // DEFAULT CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof DefaultCond) {
            return evaluate(((DefaultCond) exp).getCondition(), env);
        } //
        ////////////////////////////////////////////////////////////////////
        // IN-STATE CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof InStateCond) {
            return new BooleanValue(mInterpreter.getConfiguration().isInState(((InStateCond) exp).getState()));
        } ////////////////////////////////////////////////////////////////////
        // IN-STATE CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof PrologCond) {
            return evaluate(((PrologCond) exp).getUsrCmd(), env);
        } //
        ////////////////////////////////////////////////////////////////////
        // VALUE-OF EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof ValueOf) {
            Configuration.State state = mInterpreter.getConfiguration().getState(((ValueOf) exp).getNode());

            if (state != null) {
                return state.getThread().getEnvironment().read(((ValueOf) exp).getVar());
            } else {
                throw new InterpretException(exp,
                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // HISTORY CONTAINS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof HistoryContainsState) {
            SystemHistory.Entry entry = mInterpreter.getSystemHistory().get(((HistoryContainsState) exp).getState(),
                    ((HistoryContainsState) exp).getDepth());

            if (entry == null) {
                return new BooleanValue(false);
            } else {
                return new BooleanValue(entry.containsChildNode(((HistoryContainsState) exp).getSubState()));
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // HISTORY VALUE OF
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof HistoryValueOf) {
            return mInterpreter.getSystemHistory().get(((HistoryValueOf) exp).getNode(),
                    ((HistoryValueOf) exp).getDepth()).getValueOf(((HistoryValueOf) exp).getVar());
        } //
        ////////////////////////////////////////////////////////////////////
        // HISTORY RUNTIME
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof HistoryRunTimeOf) {
            return new IntValue((int) mInterpreter.getSystemHistory().get(((HistoryRunTimeOf) exp).getNode(),
                    ((HistoryRunTimeOf) exp).getDepth()).getRunTime());
        } //
        ////////////////////////////////////////////////////////////////////
        // USER COMMAND EXECUTION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof UsrCmd) {
            java.lang.Object result = null;
            try {
                result = executeUsrCmd((UsrCmd) exp, env);
            } catch (Exception e) {
                
                throw new InterpretException(exp, "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
            }
            if (result instanceof Boolean) {
                return new BooleanValue((Boolean) result);
            } else if (result instanceof Character) {
                return new StringValue((java.lang.String) result);
            } else if (result instanceof Short) {
                return new IntValue((Integer) result);
            } else if (result instanceof Integer) {
                return new IntValue((Integer) result);
            } else if (result instanceof Long) {
                return new LongValue((Long) result);
            } else if (result instanceof java.lang.Float) {
                return new FloatValue((java.lang.Float) result);
            } else if (result instanceof Double) {
                return new FloatValue((java.lang.Float) result);
            } else if (result instanceof Byte) {
                return new StringValue((java.lang.String) result);
            } else if (result instanceof java.lang.String) {
                return new StringValue((java.lang.String) result);
            } else {
//                java.lang.String errorMsg = "An error occured while executing thread "
//                        + Process.currentThread().toString() + " : " + "The return value '"
//                        + result + "' of the user command call '" + exp.getConcreteSyntax()
//                        + "' has an invalid type.";    /// TODO NOT BEING USED
                // throw new RunTimeException(exp, errorMsg);
                return new ObjectValue(result);
            }
        } else {
            throw new InterpretException(exp, "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
        }
    }

    /**
     *
     * Evaluate a list of expressions
     *
     */
    public LinkedList<AbstractValue> evaluateExpList(Vector<Expression> expList, Environment env)
            throws InterpretException /*
     * , InterruptException, TerminatedException
     */ {
        LinkedList<AbstractValue> valueList = new LinkedList<AbstractValue>();

        for (Expression exp : expList) {
            AbstractValue value = evaluate(exp, env);

            valueList.add(value);
        }

        return valueList;
    }

    /**
     *
     * Evaluate a list of struct member assignments
     *
     */
    public HashMap<java.lang.String, AbstractValue> evaluateAsgList(Vector<Assignment> expList, Environment env)
            throws InterpretException /*
     * , InterruptException, TerminatedException
     */ {
        HashMap<java.lang.String, AbstractValue> valueMap = new HashMap<java.lang.String, AbstractValue>();

        for (Assignment exp : expList) {
            valueMap.put(((VarExp) exp.getLExp()).getName(), evaluate(exp.getExp(), env));
        }

        return valueMap;
    }

    /**
     *
     * Execute a user command
     *
     */
    private java.lang.Object executeUsrCmd(UsrCmd cmd, Environment env) throws InterpretException, Exception {

        // Get the name of the command
        java.lang.String cmdName = ((UsrCmd) cmd).getName();

        // Evaluate the argument list of the command
        LinkedList<AbstractValue> valueList = evaluateExpList(((UsrCmd) cmd).getArgList(), env);

        // Get the user command definition of this command
        FunDef cmdDef = mInterpreter.getSceneFlow().getUsrCmdDefMap().get(cmdName);

        if (cmdDef == null) {
            java.lang.String errorMsg = "An error occured while executing thread " + Process.currentThread().toString()
                    + " : " + "The user command call '" + cmd.getConcreteSyntax()
                    + "' referes to the user command '" + cmdName + "' which is not defined.";

            throw new InterpretException(this, errorMsg);
        }

        java.lang.String cmdClassName = cmdDef.getClassName();
        java.lang.String cmdMethodName = cmdDef.getMethod();

        // Construct the parameter list of the command
        Class[] paramClassList = new Class[cmdDef.getParamList().size()];

        for (int i = 0; i < cmdDef.getParamList().size(); i++) {
            ParamDef paramDef = cmdDef.getParamList().get(i);

            // mLogger.message(paramDef.getConcreteSyntax());
            java.lang.String paramType = paramDef.getType();
            Class paramClass = null;

            if (paramType.equals("boolean")) {
                paramClass = boolean.class;
            } else if (paramType.equals("char")) {
                paramClass = char.class;
            } else if (paramType.equals("short")) {
                paramClass = short.class;
            } else if (paramType.equals("int")) {
                paramClass = int.class;
            } else if (paramType.equals("long")) {
                paramClass = long.class;
            } else if (paramType.equals("float")) {
                paramClass = float.class;
            } else if (paramType.equals("double")) {
                paramClass = double.class;
            } else if (paramType.equals("byte")) {
                paramClass = byte.class;
            } else {
                try {
                    paramClass = Class.forName(paramType);
                } catch (ClassNotFoundException e) {
                    e.printStackTrace();

                    // System.err.println(e.toString());
                }
            }

            paramClassList[i] = paramClass;

            // System.err.println("Parameter is " + paramClassList[i]);
        }

        //
        java.lang.String[] argDscrList = new java.lang.String[valueList.size()];
        java.lang.Object[] argInstList = new java.lang.Object[valueList.size()];

        for (int i = 0; i < valueList.size(); i++) {

            // System.err.println("Interpreter Value [" + i + "] Is " + valueList.get(i).getClass());
            // Get the java object of the interpreter value
            argInstList[i] = valueList.get(i).getValue();

            //
            // System.err.println("Java Argument Object [" + i + "] Is " + argInstList[i].getClass());
            //
            if (argInstList[i] != null) {
                argDscrList[i] = argInstList[i].toString();
            } else {

                // System.err.println("ATTENTION WE HAVE NULL");
                argDscrList[i] = "NULL";
            }

            // System.err.println("Argument is " + argInstList[i] + "(" + argDscrList[i]+")");
        }

        // System.err.println("Argument List is " + argList);
        // / Do the right array conversion
        for (int i = 0; i < paramClassList.length; i++) {
            if (paramClassList[i].isArray()) {

                // This parameter is an array class
                // Get the component type of the array class
                Class compType = paramClassList[i].getComponentType();

                // System.err.println("Component type is  " + compType.toString());
                // Cast the argument to an object[]
                java.lang.Object[] objArr = ((java.lang.Object[]) argInstList[i]);
                java.lang.Object myNewArray = Array.newInstance(compType, objArr.length);

                // System.err.println("New array is " + myNewArray.getClass().toString());
                for (int j = 0; j < objArr.length; j++) {
                    Array.set(myNewArray, j, compType.cast(objArr[j]));
                }

                argInstList[i] = myNewArray;
                argDscrList[i] = myNewArray.toString();

                // System.err.println("Arglist [" + i + "] is now " + myNewArray.getClass().toString());
            }
        }

        // DEBUG
        java.lang.String argListStr = "( ";

        for (int k = 0; k < argDscrList.length; k++) {
            argListStr += argDscrList[k] + " ";
        }

        argListStr += ")";

        boolean isObject = false;

        try {
            Class myClass = Class.forName(cmdClassName);

            // Invoke the method
            Method mthd = myClass.getMethod(cmdMethodName, paramClassList);

            // mLogger.message("Evaluator: Executing static Java method '" + mthd + argListStr + "'");
            try {

                // Release The Lock
                mInterpreter.unlock();

                // Invoke The Method
                return mthd.invoke(null, argInstList);
            } finally {

                // Aquire The Lock
                mInterpreter.lock();
            }
        } catch (SecurityException e) {
            e.printStackTrace();

            // System.err.println(e.toString());
        } catch (ClassNotFoundException e) {

            // e.printStackTrace();
            // System.err.println(e.toString());
        } catch (NoSuchMethodException e) {
            e.printStackTrace();

            // System.err.println(e.toString());
        } catch (IllegalAccessException e) {
            e.printStackTrace();

            // System.err.println(e.toString());
        } catch (InvocationTargetException e) {
            e.printStackTrace();

            // System.err.println(e.toString());
        }

        // We have an object
        try {
            int dotIndex = cmdClassName.lastIndexOf('.');
            java.lang.String parentClassName = cmdClassName.substring(0, dotIndex);
            java.lang.String memberFieldName = cmdClassName.substring(dotIndex + 1);

            //
            Class parentClass = Class.forName(parentClassName);

            // mLogger.message("Evaluator: Parent Class Is " + parentClass);
            Field memberField = parentClass.getField(memberFieldName);

            // mLogger.message("Evaluator: Member Field Is " + memberField);
            Class memberFieldClass = memberField.getType();

            // mLogger.message("Evaluator: Member Field  Type Is " + memberFieldClass);
            java.lang.Object memberFieldObject = memberField.get(null);

            // mLogger.message("Evaluator: Member Field Object Is " + memberFieldObject);
            // Invoke the method
            Method method = memberFieldClass.getMethod(cmdMethodName, paramClassList);

            // DEBUG
            // ATTENTION: This Lock was not released before
            // mLogger.message("Evaluator: Executing Java method '" + method + argListStr + "' on static object " + memberFieldObject);
            try {

                // Release The Lock
                mInterpreter.unlock();

                // Invoke The Method
                return method.invoke(memberFieldObject, argInstList);
            } finally {

                // Aquire The Lock
                mInterpreter.lock();
            }
        } catch (Exception e) {
            throw e;
        }
        //return null;
    }
}
