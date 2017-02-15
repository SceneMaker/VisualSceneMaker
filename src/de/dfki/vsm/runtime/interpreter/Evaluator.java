package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistoryClearFlat;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistoryClearDeep;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistorySetDepth;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActionCommand;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.UnblockSceneScript;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryRunTimeOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.JavaCallExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.NullLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryContains;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.InStateCond;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.PrologQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.TimeoutCond;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayDialogAction;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.DoubleValue;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.LongValue;
import de.dfki.vsm.runtime.interpreter.value.ObjectValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.interpreter.value.StructValue;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLUtility;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import org.jpl7.Term;

/**
 * @author Gregor Mehlmann
 */
public class Evaluator {

    // The singelton logger instance 
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The parent interpreter object
    private final Interpreter mInterpreter;

    // Construct evaluator with interpreter
    public Evaluator(final Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    // Execute a definition  
    public final void define(
            final VariableDefinition def,
            final Environment env) throws InterpreterError {
        // Create a new environment entry
        env.create(def.getName(), evaluate(def.getExp(), env));
    }

    // Execute a command 
    public final void execute(
            final Command cmd,
            final Environment env) throws InterpreterError {
        if (cmd instanceof Assignment) {
            final VariableExpression var = ((Assignment) cmd).getLeftExpression();
            final Expression exp = ((Assignment) cmd).getInitExpression();
            // Simple variable assignment
            if (var instanceof SimpleVariable) {
                env.write(((SimpleVariable) var).getName(), evaluate(exp, env));
            } // Array variable assignment
            else if (var instanceof ArrayVariable) {
                final AbstractValue value = evaluate(((ArrayVariable) var).getExpression(), env);
                if (value.getType() == AbstractValue.Type.INT) {
                    env.write(((ArrayVariable) var).getName(), ((IntValue) value).intValue(), evaluate(exp, env));
                } else {
                    throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
                }
            } // Member variable assignment
            else if (var instanceof MemberVariable) {
                env.write(((MemberVariable) var).getName(), ((MemberVariable) var).getMember(), evaluate(exp, env));
            } else {
                throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
            }
        } else if (cmd instanceof HistoryClearDeep) {
            mInterpreter.getSystemHistory().eraseDeep(
                    ((HistoryClearDeep) cmd).getState());
        } else if (cmd instanceof HistoryClearFlat) {
            mInterpreter.getSystemHistory().eraseFlat(
                    ((HistoryClearFlat) cmd).getState());
        } else if (cmd instanceof HistorySetDepth) {
            mInterpreter.getSystemHistory().setDepth(
                    ((HistorySetDepth) cmd).getState(),
                    ((HistorySetDepth) cmd).getDepth());
        } else if (cmd instanceof PlayActionCommand) {
            final PlayActionCommand command = (PlayActionCommand) cmd;
            final AbstractValue value = evaluate(command.getCommand(), env);
            final LinkedList list = evaluateExpList(command.getArgList(), env);
            // Check the type of the command
            if (value instanceof StringValue) {
                try {
                    // Unlock the interpreter
                    mInterpreter.unlock();
                    // Execute the activity
                    mInterpreter.getScenePlayer().playActionActivity(((StringValue) value).getValue(), list);
                } finally {
                    // Lock interpreter again
                    mInterpreter.lock();
                }
            } else {
                throw new InterpreterError(cmd, "Interpreter Error: '" + cmd.getConcreteSyntax() + "' cannot be executed");
            }
        } else if (cmd instanceof PlaySceneGroup) {
            final PlaySceneGroup command = (PlaySceneGroup) cmd;
            final AbstractValue value = evaluate(command.getArgument(), env);
            final LinkedList list = evaluateExpList(command.getArgList(), env);
            // Check the type of the command
            if (value instanceof StringValue) {
                try {
                    // Unlock the interpreter
                    mInterpreter.unlock();
                    // Execute the activity
                    mInterpreter.getScenePlayer().playSceneGroup(((StringValue) value).getValue(), list);
                } finally {
                    // Lock interpreter again
                    mInterpreter.lock();
                }
            } else {
                throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
            }
        } else if (cmd instanceof PlayDialogAction) {
            throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } else if (cmd instanceof UnblockSceneGroup) {
            throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } else if (cmd instanceof UnblockSceneScript) {
            throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' cannot be executed");
        } else {
            evaluate((Expression) cmd, env);
        }
    }

    // Evaluate an expression
    public final AbstractValue evaluate(
            final Expression exp,
            final Environment env) throws InterpreterError {
        if (exp instanceof BoolLiteral) {
            return new BooleanValue(((BoolLiteral) exp).getValue());
        } else if (exp instanceof IntLiteral) {
            return new IntValue(((IntLiteral) exp).getValue());
        } else if (exp instanceof FloatLiteral) {
            return new FloatValue(((FloatLiteral) exp).getValue());
        } else if (exp instanceof StringLiteral) {
            return new StringValue(((StringLiteral) exp).getValue());
        } else if (exp instanceof NullLiteral) {
            return new ObjectValue(/*((NullLiteral) exp).getValue()*/null);
        } else if (exp instanceof ArrayExpression) {
            return new ListValue(evaluateExpList(((ArrayExpression) exp).getExpList(), env));
        } else if (exp instanceof StructExpression) {
            return new StructValue(evaluateAsgList(((StructExpression) exp).getExpList(), env));
        } else if (exp instanceof BinaryExpression) {
            final BinaryExpression bin = (BinaryExpression) exp;
            final AbstractValue left = evaluate(bin.getLeftExp(), env);
            final AbstractValue right = evaluate(bin.getRightExp(), env);
            final BinaryExpression.Operator operator = bin.getOperator();
            //
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
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
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
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
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
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
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
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Mod) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() % ((IntValue) right).intValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Eq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(left.equalsValue(right));
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Neq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(!(left.equalsValue(right)));
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Ge) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).intValue() >= ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).floatValue() >= ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).longValue() >= ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).doubleValue() >= ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Gt) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).intValue() > ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).floatValue() > ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).longValue() > ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).doubleValue() > ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Le) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).intValue() <= ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).floatValue() <= ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).longValue() <= ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).doubleValue() <= ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.Lt) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new BooleanValue(((IntValue) left).intValue() < ((IntValue) right).intValue());
                } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
                    return new BooleanValue(((FloatValue) left).floatValue() < ((FloatValue) right).floatValue());
                } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
                    return new BooleanValue(((LongValue) left).longValue() < ((LongValue) right).longValue());
                } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
                    return new BooleanValue(((DoubleValue) left).doubleValue() < ((DoubleValue) right).doubleValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.AndAnd) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() && ((BooleanValue) right).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.OrOr) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() || ((BooleanValue) right).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.Operator.And) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == BinaryExpression.Operator.Or) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == BinaryExpression.Operator.Xor) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof UnaryExpression) {
            final UnaryExpression un = (UnaryExpression) exp;
            final AbstractValue value = evaluate(un.getExp(), env);
            final UnaryExpression.Operator operator = un.getOperator();
            if (operator == UnaryExpression.Operator.Neg) {
                if (value instanceof IntValue) {
                    return new IntValue(-((IntValue) value).getValue());
                } else if (value instanceof FloatValue) {
                    return new FloatValue(-((FloatValue) value).floatValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Not) {
                if (value instanceof BooleanValue) {
                    return new BooleanValue(!((BooleanValue) value).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.Operator.Lnot) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == UnaryExpression.Operator.Inc) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == UnaryExpression.Operator.Dec) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof TernaryExpression) {
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
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof SimpleVariable) {
            return env.read(((SimpleVariable) exp).getName());
        } else if (exp instanceof ArrayVariable) {
            AbstractValue index = evaluate(((ArrayVariable) exp).getExpression(), env);
            if (index.getType() == AbstractValue.Type.INT) {
                return env.read(((ArrayVariable) exp).getName(), ((IntValue) index).getValue());
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof MemberVariable) {
            return env.read(((MemberVariable) exp).getName(), ((MemberVariable) exp).getMember());
        } else if (exp instanceof TimeoutCond) {
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
        } else if (exp instanceof InStateCond) {
            return new BooleanValue(mInterpreter.getConfiguration().isInState(((InStateCond) exp).getState()));
        } else if (exp instanceof PrologQuery) {
            final AbstractValue query = evaluate(((PrologQuery) exp).getExpression(), env);
            if (query instanceof StringValue) {
                return new BooleanValue(executeQuery(((StringValue) query).getValue(), env));
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof HistoryContains) {
            SystemHistory.Entry entry = mInterpreter.getSystemHistory().get(((HistoryContains) exp).getState(),
                    ((HistoryContains) exp).getDepth());
            if (entry == null) {
                return new BooleanValue(false);
            } else {
                return new BooleanValue(entry.containsChildNode(((HistoryContains) exp).getSubState()));
            }
        } else if (exp instanceof HistoryValueOf) {
            return mInterpreter.getSystemHistory().get(((HistoryValueOf) exp).getNode(),
                    ((HistoryValueOf) exp).getDepth()).getValueOf(((HistoryValueOf) exp).getVar());
        } else if (exp instanceof HistoryRunTimeOf) {
            return new IntValue((int) mInterpreter.getSystemHistory().get(((HistoryRunTimeOf) exp).getNode(),
                    ((HistoryRunTimeOf) exp).getDepth()).getRunTime());
        } else if (exp instanceof JavaCallExpression) {
            Object result = null;
            try {
                result = executeUsrCmd((JavaCallExpression) exp, env);
            } catch (final Exception exc) {
                //e.printStackTrace();
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
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
                return new ObjectValue(result);
            }
        } else {
            throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated.");
        }

    }

    // Evaluate expression list
    public final LinkedList<AbstractValue> evaluateExpList(
            final ArrayList<Expression> list,
            final Environment env) throws InterpreterError {
        final LinkedList<AbstractValue> values = new LinkedList();
        for (final Expression exp : list) {
            values.add(evaluate(exp, env));
        }
        return values;
    }

    // Evaluate assignment list
    public final HashMap<String, AbstractValue> evaluateAsgList(
            final ArrayList<Assignment> list,
            final Environment env) throws InterpreterError {
        final HashMap<String, AbstractValue> values = new HashMap();
        for (final Assignment exp : list) {
            // ATTENTION: An assignment can incklude any type of variable expression - not just simple variables
            values.put(((SimpleVariable) exp.getLeftExpression()).getName(), evaluate(exp.getInitExpression(), env));
        }
        return values;
    }

    // Execute a Java command
    private Object executeUsrCmd(
            final JavaCallExpression cmd,
            final Environment env) throws InterpreterError, Exception {

        // Get the name of the command
        java.lang.String cmdName = ((JavaCallExpression) cmd).getName();

        // Evaluate the argument list of the command
        LinkedList<AbstractValue> list = evaluateExpList(((JavaCallExpression) cmd).getArgList(), env);

        // Get the user command definition of this command
        FunctionDefinition cmdDef = mInterpreter.getSceneFlow().getUsrCmdDefMap().get(cmdName);

        if (cmdDef == null) {
            java.lang.String errorMsg = "An error occured while executing thread " + Process.currentThread().toString()
                    + " : " + "The user command call '" + cmd.getConcreteSyntax()
                    + "' referes to the user command '" + cmdName + "' which is not defined.";

            throw new InterpreterError(this, errorMsg);
        }

        java.lang.String cmdClassName = cmdDef.getClassName();
        java.lang.String cmdMethodName = cmdDef.getMethod();

        // Construct the parameter list of the command
        Class[] paramClassList = new Class[cmdDef.getParamList().size()];

        for (int i = 0; i < cmdDef.getParamList().size(); i++) {
            ArgumentDefinition paramDef = cmdDef.getParamList().get(i);

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
        java.lang.String[] argDscrList = new java.lang.String[list.size()];
        java.lang.Object[] argInstList = new java.lang.Object[list.size()];

        for (int i = 0; i < list.size(); i++) {

            // System.err.println("Interpreter Value [" + i + "] Is " + valueList.get(i).getClass());
            // Get the java object of the interpreter value
            argInstList[i] = list.get(i).getValue();

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

        // System.err.println("Argument ListRecord is " + argList);
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

        //mLogger.warning("Executing Prolog Query '" + querystr + "'");
        // Make The Query To The KB
        final JPLResult result = JPLEngine.query(querystr);
        final JPLResult clean = result.clean();

        // Check The Query Results
        if (clean.size() == 1) {
            // Get The First And Single Substitution
            Map<String, Term> subst = clean.getFirst();
            // Try To Set The Variables Locally
            // Because A Local Thread Is Trying
            // Set The Variables In The Environment
            for (Entry<String, Term> entry : subst.entrySet()) {
                try {
                    // Get the variable name
                    final String variable = entry.getKey();
                    final Term term = entry.getValue();
                    // Convert list and pair appearances
                    final String binding = JPLUtility.convert(term.toString());

                    // mLogger.warning("Variable '" + variable + "'");
                    // mLogger.warning("Term '" + binding + "'");
                    //mLogger.warning("Setting Variable " + variable + " To " + binding + " Via Prolog Query");
                    // This call returns nothing if the variable exists and and throws an exeption
                    env.write(variable, new StringValue(binding));

                } catch (Exception exc) {

                    // Print Debug Information
                    mLogger.failure(exc.toString());
                }
            }
            return true;
        } else {
            return false;
        }
    }
}
