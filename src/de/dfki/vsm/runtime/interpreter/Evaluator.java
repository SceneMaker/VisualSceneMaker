package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistoryClearFlat;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistoryClearDeep;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.HistorySetDepth;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayActionActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.PlayScenesActivity;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.UnblockSceneScript;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.UnblockSceneGroup;
import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryRunTimeOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.CallingExpression;
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
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.InStateQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.PrologQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.TimeoutQuery;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ParenExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.ContainsList;
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
        } else if (cmd instanceof PlayActionActivity) {
            final PlayActionActivity command = (PlayActionActivity) cmd;
            final AbstractValue value = evaluate(command.getCommand(), env);
            final LinkedList list = evaluateExpList(command.getArgList(), env);
            // Check the type of the command
            if (value instanceof StringValue) {
                try {
                    // Unlock the interpreter
                    mInterpreter.unlock();
                    // Execute the activity
                    mInterpreter.getScenePlayer().playAction(((StringValue) value).getValue(), list);
                } finally {
                    // Lock interpreter again
                    mInterpreter.lock();
                }
            } else {
                throw new InterpreterError(cmd, "Interpreter Error: '" + cmd.getConcreteSyntax() + "' cannot be executed");
            }
        } else if (cmd instanceof PlayScenesActivity) {
            final PlayScenesActivity command = (PlayScenesActivity) cmd;
            final AbstractValue value = evaluate(command.getArgument(), env);
            final LinkedList list = evaluateExpList(command.getArgList(), env);
            // Check the type of the command
            if (value instanceof StringValue) {
                try {
                    // Unlock the interpreter
                    mInterpreter.unlock();
                    // Execute the activity
                    mInterpreter.getScenePlayer().playScene(((StringValue) value).getValue(), list);
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
        if (exp instanceof ParenExpression) {
            return evaluate(((ParenExpression) exp).getExp(), env);
        } else if (exp instanceof BoolLiteral) {
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
            final BinaryExpression.BinaryOp operator = bin.getOperator();
            //
            if (operator == BinaryExpression.BinaryOp.Add) {
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
            } else if (operator == BinaryExpression.BinaryOp.Sub) {
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
            } else if (operator == BinaryExpression.BinaryOp.Mul) {
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
            } else if (operator == BinaryExpression.BinaryOp.Div) {
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
            } else if (operator == BinaryExpression.BinaryOp.Mod) {
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() % ((IntValue) right).intValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.BinaryOp.Eq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(left.equalsValue(right));
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.BinaryOp.Neq) {
                if (left.getType() == right.getType()) {
                    return new BooleanValue(!(left.equalsValue(right)));
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.BinaryOp.Ge) {
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
            } else if (operator == BinaryExpression.BinaryOp.Gt) {
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
            } else if (operator == BinaryExpression.BinaryOp.Le) {
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
            } else if (operator == BinaryExpression.BinaryOp.Lt) {
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
            } else if (operator == BinaryExpression.BinaryOp.AndAnd) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() && ((BooleanValue) right).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.BinaryOp.OrOr) {
                if ((left instanceof BooleanValue) && (right instanceof BooleanValue)) {
                    return new BooleanValue(((BooleanValue) left).getValue() || ((BooleanValue) right).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == BinaryExpression.BinaryOp.And) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == BinaryExpression.BinaryOp.Or) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else if (operator == BinaryExpression.BinaryOp.Xor) {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof UnaryExpression) {
            final UnaryExpression unary = (UnaryExpression) exp;
            final AbstractValue value = evaluate(unary.getExp(), env);
            final UnaryExpression.UnaryOp operator = unary.getOperator();
            if (operator == UnaryExpression.UnaryOp.Neg) {
                if (value instanceof IntValue) {
                    return new IntValue(-((IntValue) value).getValue());
                } else if (value instanceof FloatValue) {
                    return new FloatValue(-((FloatValue) value).floatValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.UnaryOp.Not) {
                if (value instanceof BooleanValue) {
                    return new BooleanValue(!((BooleanValue) value).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.UnaryOp.Lnot) {
                if (value instanceof IntValue) {
                    return new IntValue(~((IntValue) value).getValue());
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else if (operator == UnaryExpression.UnaryOp.Inc) {
                AbstractValue result;
                if (value instanceof IntValue) {
                    result = new IntValue(((IntValue) value).getValue() + 1);
                } else if (value instanceof FloatValue) {
                    result = new FloatValue(((FloatValue) value).floatValue() + 1.0f);
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
                //
                if (unary.getExp() instanceof VariableExpression) {
                    final VariableExpression var = (VariableExpression) unary.getExp();
                    if (var instanceof SimpleVariable) {
                        env.write(((SimpleVariable) var).getName(), result);
                    } else if (var instanceof ArrayVariable) {
                        // TODO
                        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                    } else if (var instanceof MemberVariable) {
                        // TODO
                        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                    } else {
                        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                    }
                }
                //
                return result;
            } else if (operator == UnaryExpression.UnaryOp.Dec) {
                if (value instanceof IntValue) {
                    return new IntValue(((IntValue) value).getValue() - 1);
                } else if (value instanceof FloatValue) {
                    return new FloatValue(((FloatValue) value).floatValue() - 1.0f);
                } else {
                    throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
                }
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof TernaryExpression) {
            final AbstractValue condition = evaluate(((TernaryExpression) exp).getCondition(), env);
            final AbstractValue thenValue = evaluate(((TernaryExpression) exp).getThenExp(), env);
            final AbstractValue elseValue = evaluate(((TernaryExpression) exp).getElseExp(), env);
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
        } else if (exp instanceof TimeoutQuery) {
            if (mInterpreter.getTimeoutManager().contains((TimeoutQuery) exp)) {
                if (mInterpreter.getTimeoutManager().expired((TimeoutQuery) exp)) {
                    mInterpreter.getTimeoutManager().remove((TimeoutQuery) exp);
                    return new BooleanValue(true);
                } else {
                    return new BooleanValue(false);
                }
            } else {
                return new BooleanValue(true);
                //return new BooleanValue(false);
            }
        } else if (exp instanceof InStateQuery) {
            return new BooleanValue(mInterpreter.getConfiguration().isInState(((InStateQuery) exp).getState()));
        } else if (exp instanceof PrologQuery) {
            final AbstractValue query = evaluate(((PrologQuery) exp).getExpression(), env);
            if (query instanceof StringValue) {
                return new BooleanValue(executeQuery(((StringValue) query).getValue(), env));
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof ContainsList) {
            final AbstractValue listValue = evaluate(((ContainsList) exp).getListExp(), env);
            if (listValue instanceof ListValue) {
                final LinkedList<AbstractValue> list = ((ListValue)listValue).getValueList();
                //
                final AbstractValue itemValue = evaluate(((ContainsList) exp).getItemExp(), env);
                for(final AbstractValue value : list) {
                    if(value.equalsValue(itemValue)) {
                        return new BooleanValue(true);
                    }
                }
            }
            return new BooleanValue(false);            
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
        } else if (exp instanceof CallingExpression) {
            Object result = null;
            try {
                result = executeUsrCmd((CallingExpression) exp, env);
            } catch (final Exception exc) {
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
            final CallingExpression cmd,
            final Environment env) throws InterpreterError, Exception {

        // Get the name of the command
        final String command = ((CallingExpression) cmd).getName();
        // Evaluate the argument list
        final LinkedList<AbstractValue> list = evaluateExpList(((CallingExpression) cmd).getArgList(), env);
        // Get the user command definition 
        final FunctionDefinition definition = mInterpreter.getSceneFlow().getUsrCmdDefMap().get(command);
        // Check if definition does exist
        if (definition == null) {
            throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' is not defined");
        }
        // Get class and method name
        final String cmdClassName = definition.getClassName();
        final String cmdMethodName = definition.getMethod();
        // Get parameter class list
        final Class[] paramClassList = new Class[definition.getParamList().size()];
        for (int i = 0; i < definition.getParamList().size(); i++) {
            final ArgumentDefinition argument = definition.getParamList().get(i);
            final String paramType = argument.getType();
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
                } catch (final ClassNotFoundException exc) {
                    exc.printStackTrace();
                }
            }
            paramClassList[i] = paramClass;
        }

        // Get argument list
        final String[] argDscrList = new String[list.size()];
        final Object[] argInstList = new Object[list.size()];
        for (int i = 0; i < list.size(); i++) {
            // Get the java object value
            argInstList[i] = list.get(i).getValue();
            if (argInstList[i] != null) {
                argDscrList[i] = argInstList[i].toString();
            } else {
                argDscrList[i] = "NULL";
            }
        }
        // Do the right array conversion
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

        try {
            final Class clazz = Class.forName(cmdClassName);
            final Method method = clazz.getMethod(cmdMethodName, paramClassList);
            try {
                // Release The Lock
                mInterpreter.unlock();
                // Invoke The Method
                final Object result = method.invoke(null, argInstList);
                //
                mLogger.warning("Class Method Result = " + result);
                //
                return result;
            } finally {
                // Aquire The Lock
                mInterpreter.lock();
            }
        } catch (final SecurityException
                | ClassNotFoundException
                | NoSuchMethodException
                | IllegalAccessException
                | InvocationTargetException exc) {
            // Print stack trace
            mLogger.warning(exc.toString());
            exc.printStackTrace();
        }

        // We have an object
        try {
            int dotIndex = cmdClassName.lastIndexOf('.');
            final String parentClassName = cmdClassName.substring(0, dotIndex);
            final String memberFieldName = cmdClassName.substring(dotIndex + 1);

            final Class parentClass = Class.forName(parentClassName);

            final Field memberField = parentClass.getField(memberFieldName);
            final Class memberFieldClass = memberField.getType();
            final Object memberFieldObject = memberField.get(null);
            // Invoke the method
            final Method method = memberFieldClass.getMethod(cmdMethodName, paramClassList);

            try {
                // Release The Lock
                mInterpreter.unlock();
                mLogger.warning("Calling Member Method = " + method);
                // Invoke The Method
                final Object result = method.invoke(memberFieldObject, argInstList);
                //
                mLogger.warning("Member Method Result = " + result);
                //
                return result;
            } finally {
                // Aquire The Lock
                mInterpreter.lock();
            }
        } catch (final ClassNotFoundException
                | IllegalAccessException
                | IllegalArgumentException
                | NoSuchFieldException
                | NoSuchMethodException
                | SecurityException
                | InvocationTargetException exc) {
            // Print stack trace
            //exc.printStackTrace();
            mLogger.failure(exc.toString());
            // Propagate exception
            throw exc;
            //return null;
        }
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

                } catch (final InterpreterError exc) {

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
