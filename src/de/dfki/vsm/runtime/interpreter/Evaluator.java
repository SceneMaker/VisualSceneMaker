package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.language.command.Assignment;
import de.dfki.vsm.model.sceneflow.language.command.Command;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistoryFlatClear;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistoryDeepClear;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistorySetDepth;
import de.dfki.vsm.model.sceneflow.language.command.invocation.PlayDialogAct;
import de.dfki.vsm.model.sceneflow.language.command.invocation.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.language.command.invocation.UnblockSceneScript;
import de.dfki.vsm.model.sceneflow.language.command.invocation.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.language.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.HistoryRunTime;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.language.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.StateValueOf;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.ListRecord;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.NullObject;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.StructRecord;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.language.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.HistoryContains;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.InStateCond;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.PrologQuery;
import de.dfki.vsm.model.sceneflow.language.command.expression.function.TimeoutCall;
import de.dfki.vsm.model.sceneflow.language.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.language.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.language.definition.VariableDefinition;
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
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLUtility;
import de.dfki.vsm.util.log.LOGConsoleLogger;

//~--- JDK imports ------------------------------------------------------------
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Random;

/**
 * @author Not me
 */
public class Evaluator {

    // The singelton logger instance 
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
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
        else if (cmd instanceof PlayDialogAct) {

            // Try To Evaluate The Name Expression
            final AbstractValue name = evaluate(((PlayDialogAct) cmd).getArg(), env);

            // Try To Evaluate The Feature List
            LinkedList<AbstractValue> valueList = evaluateExpList(((PlayDialogAct) cmd).getArgList(), env);

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
        else if (cmd instanceof UnblockSceneScript) {
            throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } //
        ////////////////////////////////////////////////////////////////////
        // Assignment
        ////////////////////////////////////////////////////////////////////
        else if (cmd instanceof Assignment) {
            VariableExpression lexp = ((Assignment) cmd).getVariable();
            Expression rexp = ((Assignment) cmd).getInitializer();

            ////////////////////////////////////////////////////////////////////
            // Simple Variable Assignment
            ////////////////////////////////////////////////////////////////////
            if (lexp instanceof SimpleVariable) {
                env.write(((SimpleVariable) lexp).getName(), evaluate(rexp, env));
            } //
            ////////////////////////////////////////////////////////////////////
            // Array Field Assignment
            ////////////////////////////////////////////////////////////////////
            else if (lexp instanceof ArrayVariable) {
                AbstractValue fieldValue = evaluate(((ArrayVariable) lexp).getExp(), env);

                if (fieldValue.getType() == AbstractValue.Type.INT) {
                    env.write(((ArrayVariable) lexp).getName(), ((IntValue) fieldValue).getValue().intValue(),
                            evaluate(rexp, env));
                } else {
                    throw new InterpretException(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
                }
            } //
            ////////////////////////////////////////////////////////////////////
            // Member Variable Assignment
            ////////////////////////////////////////////////////////////////////
            else if (lexp instanceof MemberVariable) {
                env.write(((MemberVariable) lexp).getName(), ((MemberVariable) lexp).getMemberName(), evaluate(rexp, env));
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
        else if (cmd instanceof HistoryFlatClear) {
            mInterpreter.getSystemHistory().erase(((HistoryFlatClear) cmd).getState());
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
    public void declare(VariableDefinition def, Environment env) throws InterpretException {
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
        if (exp instanceof BoolLiteral) {
            return new BooleanValue(((BoolLiteral) exp).getValue());
        } else if (exp instanceof IntLiteral) {
            return new IntValue(((IntLiteral) exp).getValue());
        } else if (exp instanceof FloatLiteral) {
            return new FloatValue(((FloatLiteral) exp).getValue());
        } else if (exp instanceof StringLiteral) {
            return new StringValue(((StringLiteral) exp).getValue());
        } else if (exp instanceof NullObject) {
            return new ObjectValue(((NullObject) exp).getValue());
        } else if (exp instanceof ListRecord) {
            return new ListValue(evaluateExpList(((ListRecord) exp).getExpList(), env));
        } else if (exp instanceof StructRecord) {
            return new StructValue(evaluateAsgList(((StructRecord) exp).getExpList(), env));
        } //
        ////////////////////////////////////////////////////////////////////
        // BINARY EXPRESSIONS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof BinaryExpression) {
            AbstractValue left = evaluate(((BinaryExpression) exp).getLeftExp(), env);
            AbstractValue right = evaluate(((BinaryExpression) exp).getRightExp(), env);
            BinaryExpression.Operator operator = ((BinaryExpression) exp).getOperator();

            if (operator == BinaryExpression.Operator.Add) {
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
            } else if (operator == BinaryExpression.Operator.Sub) {
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
            } else if (operator == BinaryExpression.Operator.Mul) {
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
            } else if (operator == BinaryExpression.Operator.Div) {
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
            } else if (operator == BinaryExpression.Operator.AddFirst) {
                if (left instanceof ListValue) {
                    // Convert The Left Expression
                    final ListValue listValue = (ListValue) left;
                    // Get The Java List From Value
                    final LinkedList<AbstractValue> list = listValue.getValueList();
                    // Add The Abstract Value Here
                    list.addFirst(right);

                    return left;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.AddLast) {
                if (left instanceof ListValue) {
                    ((ListValue) left).getValueList().addLast(right);

                    return left;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Contains) {
                if (left instanceof ListValue) {
                    for (AbstractValue value : ((ListValue) left).getValueList()) {
                        if (value.getType() == right.getType()) {
                            if (value.equalsValue(right)) {
                                return new BooleanValue(true);
                            }
                        }
                    }
                    return new BooleanValue(false);
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Get) {
                if ((left instanceof ListValue) && (right instanceof IntValue)) {
                    return ((ListValue) left).getValueList().get(((IntValue) right).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Remove) {
                if ((left instanceof ListValue) && (right instanceof IntValue)) {
                    return ((ListValue) left).getValueList().remove(((IntValue) right).getValue().intValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.And) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() && ((BooleanValue) right).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Or) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() || ((BooleanValue) right).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Eq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(left.equalsValue(right));
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
                }
            } else if (operator == BinaryExpression.Operator.Neq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(!(left.equalsValue(right)));
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
                }
            } else if (operator == BinaryExpression.Operator.Ge) {
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
            } else if (operator == BinaryExpression.Operator.Gt) {
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
            } else if (operator == BinaryExpression.Operator.Le) {
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
            } else if (operator == BinaryExpression.Operator.Lt) {
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
        // UNARY EXPRESSIONS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof UnaryExpression) {
            AbstractValue value = evaluate(((UnaryExpression) exp).getExp(), env);
            UnaryExpression.Operator operator = ((UnaryExpression) exp).getOperator();

            if (operator == UnaryExpression.Operator.Size) {
                if (value instanceof ListValue) {
                    return new IntValue(((ListValue) value).getValueList().size());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Clear) {
                if (value instanceof ListValue) {
                    ((ListValue) value).getValueList().clear();

                    return value;
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.First) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().getFirst();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Last) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().getLast();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.RemoveFirst) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().removeFirst();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.RemoveLast) {
                if (value instanceof ListValue) {
                    return ((ListValue) value).getValueList().removeLast();
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Empty) {
                if (value instanceof ListValue) {
                    return new BooleanValue(((ListValue) value).isEmpty());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Random) {
                if (value instanceof IntValue) {
                    int random = new Random().nextInt(((IntValue) value).getValue().intValue());
                    return new IntValue(random);
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Neg) {
                if (value instanceof IntValue) {
                    return new IntValue(-((IntValue) value).getValue());
                } else if (value instanceof FloatValue) {
                    return new FloatValue(-((FloatValue) value).floatValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Not) {
                if (value instanceof BooleanValue) {
                    return new BooleanValue(!((BooleanValue) value).getValue());
                } else {
                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // CONDITIONAL EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof TernaryExpression) {
            AbstractValue condition = evaluate(((TernaryExpression) exp).getCondition(), env);
            AbstractValue thenValue = evaluate(((TernaryExpression) exp).getThenExp(), env);
            AbstractValue elseValue = evaluate(((TernaryExpression) exp).getElseExp(), env);

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
        else if (exp instanceof SimpleVariable) {
            return env.read(((SimpleVariable) exp).getName());
        } //
        ////////////////////////////////////////////////////////////////////
        // ARRAY VARIABLE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof ArrayVariable) {
            AbstractValue index = evaluate(((ArrayVariable) exp).getExp(), env);

            if (index.getType() == AbstractValue.Type.INT) {
                return env.read(((ArrayVariable) exp).getName(), ((IntValue) index).getValue());
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // MEMBER VARIABLE EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof MemberVariable) {
            return env.read(((MemberVariable) exp).getName(), ((MemberVariable) exp).getMemberName());
        } //
        ////////////////////////////////////////////////////////////////////
        // BINARY CONDITIONS
        ////////////////////////////////////////////////////////////////////
        //        else if (exp instanceof BinaryCond) {
        //
        //            // System.err.println("Condition= " + exp.getAbstractSyntax());
        //            AbstractValue left = evaluate(((BinaryCond) exp).getLeftCond(), env);
        //            AbstractValue right = evaluate(((BinaryCond) exp).getRightCond(), env);
        //            BinaryCond.Operator operator = ((BinaryCond) exp).getOperator();
        //
        //            if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
        //                if (operator == BinaryCond.Operator.And) {
        //                    return new BooleanValue(((BooleanValue) left).getValue() && ((BooleanValue) right).getValue());
        //                } else if (operator == BinaryCond.Operator.Or) {
        //                    return new BooleanValue(((BooleanValue) left).getValue() || ((BooleanValue) right).getValue());
        //                } else {
        //                    throw new InterpretException(exp,
        //                            "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
        //                }
        //            } else {
        //                throw new InterpretException(exp,
        //                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
        //            }
        //        } 
        //        else if (exp instanceof UnaryCond) {
        //
        //            /**
        //             * UNARY CONDITIONS
        //             */
        //            AbstractValue value = evaluate(((UnaryCond) exp).getCondition(), env);
        //            UnaryCond.Operator operator = ((UnaryCond) exp).getOperator();
        //
        //            if (value instanceof BooleanValue) {
        //                if (operator == UnaryCond.Operator.Not) {
        //                    return new BooleanValue(!((BooleanValue) value).getValue());
        //                } else {
        //                    throw new InterpretException(exp,
        //                            "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
        //                }
        //            } else {
        //                throw new InterpretException(exp,
        //                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
        //            }
        //        } 
        //        else if (exp instanceof ComparisionCond) {
        //
        //            /**
        //             * COMPARISION CONDITIONS
        //             */
        //            AbstractValue left = evaluate(((ComparisionCond) exp).getLeftExp(), env);
        //            AbstractValue right = evaluate(((ComparisionCond) exp).getRightExp(), env);
        //            ComparisionCond.Operator operator = ((ComparisionCond) exp).getOperator();
        //
        //            if (operator == ComparisionCond.Operator.Eq) {
        //                if (left.getType() == right.getType()) {
        //                    return new BooleanValue(left.equalsValue(right));
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
        //                }
        //            }
        //
        //            if (operator == ComparisionCond.Operator.Neq) {
        //                if (left.getType() == right.getType()) {
        //                    return new BooleanValue(!(left.equalsValue(right)));
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
        //                }
        //            } else if (operator == ComparisionCond.Operator.Ge) {
        //                if ((left instanceof IntValue) && (right instanceof IntValue)) {
        //                    return new BooleanValue(((IntValue) left).getValue().intValue()
        //                            >= ((IntValue) right).getValue().intValue());
        //                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
        //                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
        //                            >= ((FloatValue) right).getValue().floatValue());
        //                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
        //                    return new BooleanValue(((LongValue) left).getValue().longValue()
        //                            >= ((LongValue) right).getValue().longValue());
        //                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
        //                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
        //                            >= ((DoubleValue) right).getValue().doubleValue());
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
        //                }
        //            } else if (operator == ComparisionCond.Operator.Gt) {
        //                if ((left instanceof IntValue) && (right instanceof IntValue)) {
        //                    return new BooleanValue(((IntValue) left).getValue().intValue()
        //                            > ((IntValue) right).getValue().intValue());
        //                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
        //                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
        //                            > ((FloatValue) right).getValue().floatValue());
        //                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
        //                    return new BooleanValue(((LongValue) left).getValue().longValue()
        //                            > ((LongValue) right).getValue().longValue());
        //                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
        //                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
        //                            > ((DoubleValue) right).getValue().doubleValue());
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
        //                }
        //            } else if (operator == ComparisionCond.Operator.Le) {
        //                if ((left instanceof IntValue) && (right instanceof IntValue)) {
        //                    return new BooleanValue(((IntValue) left).getValue().intValue()
        //                            <= ((IntValue) right).getValue().intValue());
        //                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
        //                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
        //                            <= ((FloatValue) right).getValue().floatValue());
        //                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
        //                    return new BooleanValue(((LongValue) left).getValue().longValue()
        //                            <= ((LongValue) right).getValue().longValue());
        //                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
        //                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
        //                            <= ((DoubleValue) right).getValue().doubleValue());
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
        //                }
        //            } else if (operator == ComparisionCond.Operator.Lt) {
        //                if ((left instanceof IntValue) && (right instanceof IntValue)) {
        //                    return new BooleanValue(((IntValue) left).getValue().intValue()
        //                            < ((IntValue) right).getValue().intValue());
        //                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
        //                    return new BooleanValue(((FloatValue) left).getValue().floatValue()
        //                            < ((FloatValue) right).getValue().floatValue());
        //                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
        //                    return new BooleanValue(((LongValue) left).getValue().longValue()
        //                            < ((LongValue) right).getValue().longValue());
        //                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
        //                    return new BooleanValue(((DoubleValue) left).getValue().doubleValue()
        //                            < ((DoubleValue) right).getValue().doubleValue());
        //                } else {
        //                    throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
        //                }
        //            } else {
        //                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
        //            }
        //        } 
        // TIMEOUT CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof TimeoutCall) {
            if (mInterpreter.getTimeoutManager().contains((TimeoutCall) exp)) {
                if (mInterpreter.getTimeoutManager().expired((TimeoutCall) exp)) {
                    mInterpreter.getTimeoutManager().remove((TimeoutCall) exp);

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
        //        else if (exp instanceof DefaultCond) {
        //            return evaluate(((DefaultCond) exp).getCondition(), env);
        //        } //
        ////////////////////////////////////////////////////////////////////
        // IN-STATE CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof InStateCond) {
            return new BooleanValue(mInterpreter.getConfiguration().isInState(((InStateCond) exp).getState()));
        } ////////////////////////////////////////////////////////////////////
        // IN-STATE CONDITION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof PrologQuery) {
            final AbstractValue query = evaluate(((PrologQuery) exp).getExpression(), env);
            if (query instanceof StringValue) {
                return new BooleanValue(executeQuery(((StringValue) query).getValue(), env));
            } else {
                throw new InterpretException(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
            //return evaluate(((PrologQuery) exp).getExpression(), env);
        } //
        ////////////////////////////////////////////////////////////////////
        // VALUE-OF EXPRESSION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof StateValueOf) {
            Configuration.State state = mInterpreter.getConfiguration().getState(((StateValueOf) exp).getNode());

            if (state != null) {
                return state.getThread().getEnvironment().read(((StateValueOf) exp).getVar());
            } else {
                throw new InterpretException(exp,
                        "Runtime Error: '" + exp.getAbstractSyntax() + "' cannot be evaluated.");
            }
        } //
        ////////////////////////////////////////////////////////////////////
        // HISTORY CONTAINS
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof HistoryContains) {
            SystemHistory.Entry entry = mInterpreter.getSystemHistory().get(((HistoryContains) exp).getState(),
                    ((HistoryContains) exp).getDepth());

            if (entry == null) {
                return new BooleanValue(false);
            } else {
                return new BooleanValue(entry.containsChildNode(((HistoryContains) exp).getSubState()));
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
        else if (exp instanceof HistoryRunTime) {
            return new IntValue((int) mInterpreter.getSystemHistory().get(((HistoryRunTime) exp).getNode(),
                    ((HistoryRunTime) exp).getDepth()).getRunTime());
        } //
        ////////////////////////////////////////////////////////////////////
        // USER COMMAND EXECUTION
        ////////////////////////////////////////////////////////////////////
        else if (exp instanceof CallingExpression) {
            java.lang.Object result = null;
            try {
                result = executeUsrCmd((CallingExpression) exp, env);
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
    public LinkedList<AbstractValue> evaluateExpList(ArrayList<Expression> expList, Environment env)
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
    public HashMap<java.lang.String, AbstractValue> evaluateAsgList(ArrayList<Assignment> expList, Environment env)
            throws InterpretException /*
     * , InterruptException, TerminatedException
     */ {
        HashMap<java.lang.String, AbstractValue> valueMap = new HashMap<java.lang.String, AbstractValue>();

        for (Assignment exp : expList) {
            valueMap.put(((SimpleVariable) exp.getVariable()).getName(), evaluate(exp.getInitializer(), env));
        }

        return valueMap;
    }

    /**
     *
     * Execute a user command
     *
     */
    private java.lang.Object executeUsrCmd(CallingExpression cmd, Environment env) throws InterpretException, Exception {

        // Get the name of the command
        java.lang.String cmdName = ((CallingExpression) cmd).getName();

        // Evaluate the argument list of the command
        LinkedList<AbstractValue> valueList = evaluateExpList(((CallingExpression) cmd).getArgList(), env);

        // Get the user command definition of this command
        FunctionDefinition cmdDef = mInterpreter.getSceneFlow().getUsrCmdDefMap().get(cmdName);

        if (cmdDef == null) {
            java.lang.String errorMsg = "An error occured while executing thread " + Process.currentThread().toString()
                    + " : " + "The user command call '" + cmd.getConcreteSyntax()
                    + "' referes to the user command '" + cmdName + "' which is not defined.";

            throw new InterpretException(this, errorMsg);
        }

        java.lang.String cmdClassName = cmdDef.getClazz();
        java.lang.String cmdMethodName = cmdDef.getMethod();

        // Construct the parameter list of the command
        Class[] paramClassList = new Class[cmdDef.getArgList().size()];

        for (int i = 0; i < cmdDef.getArgList().size(); i++) {
            final ArgumentDefinition argDef = cmdDef.getArgAt(i);

            // mLogger.message(paramDef.getConcreteSyntax());
            java.lang.String paramType = argDef.getType();
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

    public final boolean executeQuery(final String querystr, final Environment env) {

        // Make The Query To The KB
        final JPLResult result = JPLEngine.query(querystr);

        // Check The Query Results
        if (result.size() == 1) {

            // Get The First And Single Substitution
            HashMap<String, String> subst = result.getFirst();

            // Try To Set The Variables Locally
            // Because A Local Thread Is Trying
//            try {
            // Set The Variables In The Environment
            for (Map.Entry<String, String> entry : subst.entrySet()) {
                try {

                    mLogger.failure("Setting Variable " + entry.getKey() + " To " + entry.getValue() + " Via Prolog Query");

                    // This call returns nothing if the variable exists and and throws an exeption
                    env.write(entry.getKey(), new StringValue(JPLUtility.convert(entry.getValue())));

                } catch (Exception exc) {

                    // Print Debug Information
                    mLogger.failure(exc.toString());
                }
            }
//            } 
//            catch (Exception exc) {
//
//                // Try To Set The Variables Globally
//                // Because An Extern Thread Is Trying
//                for (Map.Entry<String, String> entry : subst.entrySet()) {
//                    RunTimeInstance.getInstance().setVariable(mProject, entry.getKey(), entry.getValue());
//                }
//            }

            return true;
        } else {
            return false;
        }
    }
}
