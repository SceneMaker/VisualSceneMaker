package de.dfki.vsm.runtime.interpreter;

import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.model.sceneflow.glue.command.Assignment;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.definition.ArgumentDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.expression.*;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.*;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.*;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.glue.command.expression.variable.SimpleVariable;
import de.dfki.vsm.model.sceneflow.glue.command.invocation.*;
import de.dfki.vsm.runtime.interpreter.error.InterpreterError;
import de.dfki.vsm.runtime.interpreter.error.SceneDoesNotExists;
import de.dfki.vsm.runtime.interpreter.event.TerminationEvent;
import de.dfki.vsm.runtime.interpreter.value.*;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLUtility;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.jpl7.Term;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public final class Evaluator {

    // The singelton logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The parent interpreter object
    private final Interpreter mInterpreter;

    // Cache for resolved reflection methods to avoid Class.forName()+getMethod() per call
    private static final ConcurrentHashMap<String, ResolvedMethod> sMethodCache = new ConcurrentHashMap<>();

    private static final class ResolvedMethod {
        final Method method;
        final Object target; // null for static methods
        final Class<?>[] paramTypes;

        ResolvedMethod(Method method, Object target, Class<?>[] paramTypes) {
            this.method = method;
            this.target = target;
            this.paramTypes = paramTypes;
        }
    }

    // Construct evaluator with interpreter
    public Evaluator(final Interpreter interpreter) {
        mInterpreter = interpreter;
    }

    // Pattern for parsing Event type: Event, Event(Type), Event(Type, Capacity), Event(*, Capacity)
    private static final Pattern EVENT_TYPE_PATTERN = Pattern.compile(
            "^Event(?:\\(([^,)]*?)(?:,\\s*(\\d+))?\\))?$", Pattern.CASE_INSENSITIVE);

    // Execute a definition
    public final void define(
            final VariableDefinition def,
            final Environment env) throws InterpreterError {
        // Event variables start with an empty queue
        String typeStr = def.getType() != null ? def.getType().trim() : "";
        if (typeStr.toLowerCase().startsWith("event")) {
            int capacity = 0;
            String elementType = null;
            Matcher m = EVENT_TYPE_PATTERN.matcher(typeStr);
            if (m.matches()) {
                String et = m.group(1);
                if (et != null) {
                    et = et.trim();
                    if (!et.isEmpty() && !et.equals("*")) {
                        elementType = et;
                    }
                }
                if (m.group(2) != null) {
                    capacity = Integer.parseInt(m.group(2));
                }
            }
            env.create(def.getName(), new EventValue(capacity, elementType));
        } else {
            env.create(def.getName(), evaluate(def.getExp(), env));
        }
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
            // Variable was modified — mark interruptor dirty for re-evaluation
            mInterpreter.markInterruptorDirty();
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
            final List<AbstractValue> list = evaluateExpList(command.getArgList(), env);
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
            final List<AbstractValue> list = evaluateExpList(command.getArgList(), env);
            // Check the type of the command
            if (value instanceof StringValue) {
                try {
                    // Unlock the interpreter
                    mInterpreter.unlock();
                    // Execute the activity
                    mInterpreter.getScenePlayer().playScene(((StringValue) value).getValue(), list);
                } catch (SceneDoesNotExists missingScene) {
                    EventDispatcher.getInstance().convey(new TerminationEvent(new Object(), missingScene));
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
            return evaluateBinary((BinaryExpression) exp, env);
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
            if (condition instanceof BooleanValue) {
                // Short-circuit: only evaluate the taken branch
                return ((BooleanValue) condition).getValue()
                        ? evaluate(((TernaryExpression) exp).getThenExp(), env)
                        : evaluate(((TernaryExpression) exp).getElseExp(), env);
            } else {
                throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
            }
        } else if (exp instanceof SimpleVariable) {
            AbstractValue val = env.read(((SimpleVariable) exp).getName());
            // Event variables: consume-on-evaluate (like TimeoutQuery)
            if (val instanceof EventValue) {
                EventValue ev = (EventValue) val;
                if (!ev.isEmpty()) {
                    ev.dequeue();
                    return new BooleanValue(true);
                } else {
                    return new BooleanValue(false);
                }
            }
            return val;
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
                final List<AbstractValue> list = ((ListValue) listValue).getValueList();
                //
                final AbstractValue itemValue = evaluate(((ContainsList) exp).getItemExp(), env);
                for (final AbstractValue value : list) {
                    if (value.equalsValue(itemValue)) {
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

    // Evaluate a binary expression with short-circuit logic and switch dispatch
    private AbstractValue evaluateBinary(
            final BinaryExpression bin,
            final Environment env) throws InterpreterError {
        final BinaryExpression.BinaryOp operator = bin.getOperator();

        // Short-circuit: && and || evaluate left first, skip right if result is determined
        if (operator == BinaryExpression.BinaryOp.AndAnd) {
            final AbstractValue left = evaluate(bin.getLeftExp(), env);
            if (!(left instanceof BooleanValue)) {
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            }
            if (!((BooleanValue) left).getValue()) {
                return new BooleanValue(false); // short-circuit: false && _ = false
            }
            final AbstractValue right = evaluate(bin.getRightExp(), env);
            if (!(right instanceof BooleanValue)) {
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            }
            return right;
        }
        if (operator == BinaryExpression.BinaryOp.OrOr) {
            final AbstractValue left = evaluate(bin.getLeftExp(), env);
            if (!(left instanceof BooleanValue)) {
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            }
            if (((BooleanValue) left).getValue()) {
                return new BooleanValue(true); // short-circuit: true || _ = true
            }
            final AbstractValue right = evaluate(bin.getRightExp(), env);
            if (!(right instanceof BooleanValue)) {
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            }
            return right;
        }

        // Event variable comparisons: peek + compare + conditional consume.
        // Must happen before eager evaluation of both sides.
        if (operator == BinaryExpression.BinaryOp.Eq
                || operator == BinaryExpression.BinaryOp.Neq) {
            BooleanValue eventResult = tryEventComparison(bin, env);
            if (eventResult != null) {
                return eventResult;
            }
        }

        // Eagerly evaluate both operands for all remaining operators
        final AbstractValue left = evaluate(bin.getLeftExp(), env);
        final AbstractValue right = evaluate(bin.getRightExp(), env);

        switch (operator) {
            case Add:
                return evaluateAdd(left, right, bin);
            case Sub:
                return evaluateArithmetic(left, right, bin, operator);
            case Mul:
                return evaluateArithmetic(left, right, bin, operator);
            case Div:
                return evaluateArithmetic(left, right, bin, operator);
            case Mod:
                if ((left instanceof IntValue) && (right instanceof IntValue)) {
                    return new IntValue(((IntValue) left).intValue() % ((IntValue) right).intValue());
                }
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            case Eq:
                if (left.getType() == right.getType()) {
                    return new BooleanValue(left.equalsValue(right));
                }
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            case Neq:
                if (left.getType() == right.getType()) {
                    return new BooleanValue(!left.equalsValue(right));
                }
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
            case Ge:
                return evaluateComparison(left, right, bin, operator);
            case Gt:
                return evaluateComparison(left, right, bin, operator);
            case Le:
                return evaluateComparison(left, right, bin, operator);
            case Lt:
                return evaluateComparison(left, right, bin, operator);
            default:
                throw new InterpreterError(bin, "'" + bin.getConcreteSyntax() + "' cannot be evaluated");
        }
    }

    // Evaluate Add with string concatenation support
    private AbstractValue evaluateAdd(
            final AbstractValue left,
            final AbstractValue right,
            final Expression exp) throws InterpreterError {
        if ((left instanceof IntValue) && (right instanceof IntValue)) {
            return new IntValue(((IntValue) left).intValue() + ((IntValue) right).intValue());
        } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
            return new FloatValue(((FloatValue) left).floatValue() + ((FloatValue) right).floatValue());
        } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
            return new LongValue(((LongValue) left).longValue() + ((LongValue) right).longValue());
        } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
            return new DoubleValue(((DoubleValue) left).doubleValue() + ((DoubleValue) right).doubleValue());
        } else if (left instanceof StringValue || right instanceof StringValue) {
            // String concatenation: any type can be concatenated with a String
            return new StringValue(valueToString(left) + valueToString(right));
        }
        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
    }

    // Convert a value to its string representation for concatenation
    private static String valueToString(AbstractValue val) {
        if (val instanceof StringValue) return ((StringValue) val).getValue();
        if (val instanceof IntValue) return String.valueOf(((IntValue) val).intValue());
        if (val instanceof FloatValue) return String.valueOf(((FloatValue) val).floatValue());
        if (val instanceof BooleanValue) return String.valueOf(((BooleanValue) val).booleanValue());
        if (val instanceof LongValue) return String.valueOf(((LongValue) val).longValue());
        return String.valueOf(val.getValue());
    }

    // Evaluate arithmetic operators (Sub, Mul, Div) on numeric types
    private AbstractValue evaluateArithmetic(
            final AbstractValue left,
            final AbstractValue right,
            final Expression exp,
            final BinaryExpression.BinaryOp op) throws InterpreterError {
        if ((left instanceof IntValue) && (right instanceof IntValue)) {
            int l = ((IntValue) left).intValue(), r = ((IntValue) right).intValue();
            switch (op) {
                case Sub: return new IntValue(l - r);
                case Mul: return new IntValue(l * r);
                case Div: return new IntValue(l / r);
                default: break;
            }
        } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
            float l = ((FloatValue) left).floatValue(), r = ((FloatValue) right).floatValue();
            switch (op) {
                case Sub: return new FloatValue(l - r);
                case Mul: return new FloatValue(l * r);
                case Div: return new FloatValue(l / r);
                default: break;
            }
        } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
            long l = ((LongValue) left).longValue(), r = ((LongValue) right).longValue();
            switch (op) {
                case Sub: return new LongValue(l - r);
                case Mul: return new LongValue(l * r);
                case Div: return new LongValue(l / r);
                default: break;
            }
        } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
            double l = ((DoubleValue) left).doubleValue(), r = ((DoubleValue) right).doubleValue();
            switch (op) {
                case Sub: return new DoubleValue(l - r);
                case Mul: return new DoubleValue(l * r);
                case Div: return new DoubleValue(l / r);
                default: break;
            }
        }
        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
    }

    // Evaluate comparison operators (Ge, Gt, Le, Lt) on numeric types
    private AbstractValue evaluateComparison(
            final AbstractValue left,
            final AbstractValue right,
            final Expression exp,
            final BinaryExpression.BinaryOp op) throws InterpreterError {
        if ((left instanceof IntValue) && (right instanceof IntValue)) {
            int l = ((IntValue) left).intValue(), r = ((IntValue) right).intValue();
            switch (op) {
                case Ge: return new BooleanValue(l >= r);
                case Gt: return new BooleanValue(l > r);
                case Le: return new BooleanValue(l <= r);
                case Lt: return new BooleanValue(l < r);
                default: break;
            }
        } else if ((left instanceof FloatValue) && (right instanceof FloatValue)) {
            float l = ((FloatValue) left).floatValue(), r = ((FloatValue) right).floatValue();
            switch (op) {
                case Ge: return new BooleanValue(l >= r);
                case Gt: return new BooleanValue(l > r);
                case Le: return new BooleanValue(l <= r);
                case Lt: return new BooleanValue(l < r);
                default: break;
            }
        } else if ((left instanceof LongValue) && (right instanceof LongValue)) {
            long l = ((LongValue) left).longValue(), r = ((LongValue) right).longValue();
            switch (op) {
                case Ge: return new BooleanValue(l >= r);
                case Gt: return new BooleanValue(l > r);
                case Le: return new BooleanValue(l <= r);
                case Lt: return new BooleanValue(l < r);
                default: break;
            }
        } else if ((left instanceof DoubleValue) && (right instanceof DoubleValue)) {
            double l = ((DoubleValue) left).doubleValue(), r = ((DoubleValue) right).doubleValue();
            switch (op) {
                case Ge: return new BooleanValue(l >= r);
                case Gt: return new BooleanValue(l > r);
                case Le: return new BooleanValue(l <= r);
                case Lt: return new BooleanValue(l < r);
                default: break;
            }
        }
        throw new InterpreterError(exp, "'" + exp.getConcreteSyntax() + "' cannot be evaluated");
    }

    // Evaluate expression list
    public final List<AbstractValue> evaluateExpList(
            final ArrayList<Expression> list,
            final Environment env) throws InterpreterError {
        final LinkedList<AbstractValue> values = new LinkedList<>();
        for (final Expression exp : list) {
            values.add(evaluate(exp, env));
        }
        return values;
    }

    // Evaluate assignment list
    public final Map<String, AbstractValue> evaluateAsgList(
            final ArrayList<Assignment> list,
            final Environment env) throws InterpreterError {
        final HashMap<String, AbstractValue> values = new HashMap<>();
        for (final Assignment exp : list) {
            // ATTENTION: An assignment can incklude any type of variable expression - not just simple variables
            values.put(((SimpleVariable) exp.getLeftExpression()).getName(), evaluate(exp.getInitExpression(), env));
        }
        return values;
    }

    // Resolve parameter types for a function definition (used for reflection)
    private static Class<?>[] resolveParamTypes(FunctionDefinition definition) throws ClassNotFoundException {
        final Class<?>[] paramClassList = new Class<?>[definition.getParamList().size()];
        for (int i = 0; i < definition.getParamList().size(); i++) {
            final ArgumentDefinition argument = definition.getParamList().get(i);
            final String paramType = argument.getType();
            switch (paramType) {
                case "boolean": paramClassList[i] = boolean.class; break;
                case "char":    paramClassList[i] = char.class; break;
                case "short":   paramClassList[i] = short.class; break;
                case "int":     paramClassList[i] = int.class; break;
                case "long":    paramClassList[i] = long.class; break;
                case "float":   paramClassList[i] = float.class; break;
                case "double":  paramClassList[i] = double.class; break;
                case "byte":    paramClassList[i] = byte.class; break;
                default:        paramClassList[i] = Class.forName(paramType); break;
            }
        }
        return paramClassList;
    }

    // Build a cache key from class name, method name, and parameter types
    private static String buildMethodCacheKey(FunctionDefinition def) {
        StringBuilder sb = new StringBuilder(def.getClassName());
        sb.append('#').append(def.getMethod()).append('(');
        for (int i = 0; i < def.getParamList().size(); i++) {
            if (i > 0) sb.append(',');
            sb.append(def.getParamList().get(i).getType());
        }
        sb.append(')');
        return sb.toString();
    }

    // Resolve a method for a function definition (try static class method, then member field method)
    private static ResolvedMethod resolveMethod(FunctionDefinition definition) throws Exception {
        final String cmdClassName = definition.getClassName();
        final String cmdMethodName = definition.getMethod();
        final Class<?>[] paramTypes = resolveParamTypes(definition);

        // Path 1: Static class method
        try {
            final Class<?> clazz = Class.forName(cmdClassName);
            final Method method = clazz.getMethod(cmdMethodName, paramTypes);
            return new ResolvedMethod(method, null, paramTypes);
        } catch (final ClassNotFoundException | NoSuchMethodException ignored) {
            // Fall through to Path 2
        }

        // Path 2: Instance method on a static field (e.g., "pkg.Class.field" -> field.method())
        int dotIndex = cmdClassName.lastIndexOf('.');
        final String parentClassName = cmdClassName.substring(0, dotIndex);
        final String memberFieldName = cmdClassName.substring(dotIndex + 1);
        final Class<?> parentClass = Class.forName(parentClassName);
        final Field memberField = parentClass.getField(memberFieldName);
        final Class<?> memberFieldClass = memberField.getType();
        final Object memberFieldObject = memberField.get(null);
        final Method method = memberFieldClass.getMethod(cmdMethodName, paramTypes);
        return new ResolvedMethod(method, memberFieldObject, paramTypes);
    }

    // Execute a Java command
    private Object executeUsrCmd(
            final CallingExpression cmd,
            final Environment env) throws Exception {

        // Get the name of the command
        final String command = cmd.getName();
        // Evaluate the argument list
        final List<AbstractValue> list = evaluateExpList(cmd.getArgList(), env);
        // Get the user command definition
        final FunctionDefinition definition = mInterpreter.getSceneFlow().getUsrCmdDefMap().get(command);
        // Check if definition does exist
        if (definition == null) {
            throw new InterpreterError(cmd, "'" + cmd.getConcreteSyntax() + "' is not defined");
        }

        // Resolve method from cache or via reflection
        final String cacheKey = buildMethodCacheKey(definition);
        ResolvedMethod resolved = sMethodCache.get(cacheKey);
        if (resolved == null) {
            resolved = resolveMethod(definition);
            sMethodCache.put(cacheKey, resolved);
        }

        // Get argument list
        final Object[] argInstList = new Object[list.size()];
        for (int i = 0; i < list.size(); i++) {
            argInstList[i] = list.get(i).getValue();
        }
        // Do the right array conversion
        final Class<?>[] paramTypes = resolved.paramTypes;
        for (int i = 0; i < paramTypes.length; i++) {
            if (paramTypes[i].isArray()) {
                Class<?> compType = paramTypes[i].getComponentType();
                java.lang.Object[] objArr = ((java.lang.Object[]) argInstList[i]);
                java.lang.Object myNewArray = Array.newInstance(compType, objArr.length);
                for (int j = 0; j < objArr.length; j++) {
                    Array.set(myNewArray, j, compType.cast(objArr[j]));
                }
                argInstList[i] = myNewArray;
            }
        }

        try {
            // Release The Lock
            mInterpreter.unlock();
            // Invoke The Method
            final Object result = resolved.method.invoke(resolved.target, argInstList);
            return result;
        } finally {
            // Acquire The Lock
            mInterpreter.lock();
        }
    }

    private final boolean executeQuery(final String querystr, final Environment env) {

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

    /**
     * Check if a binary == or != expression involves an event variable.
     * If so, peek at the queue front, compare with the other operand,
     * and consume only when the comparison condition is satisfied.
     *
     * Returns null if neither side is an event variable (caller falls
     * through to normal evaluation).
     */
    private BooleanValue tryEventComparison(
            final BinaryExpression bin,
            final Environment env) throws InterpreterError {
        final boolean isEq = bin.getOperator() == BinaryExpression.BinaryOp.Eq;
        // Check left side
        EventValue leftEvent = resolveEventVariable(bin.getLeftExp(), env);
        if (leftEvent != null) {
            AbstractValue right = evaluate(bin.getRightExp(), env);
            return evaluateEventCompare(leftEvent, right, isEq);
        }
        // Check right side
        EventValue rightEvent = resolveEventVariable(bin.getRightExp(), env);
        if (rightEvent != null) {
            AbstractValue left = evaluate(bin.getLeftExp(), env);
            return evaluateEventCompare(rightEvent, left, isEq);
        }
        return null;
    }

    /**
     * Resolve a SimpleVariable expression to its EventValue without
     * going through evaluate() (which would auto-consume).
     * Returns null if the expression is not a SimpleVariable or its
     * value is not an EventValue.
     */
    private EventValue resolveEventVariable(
            final Expression exp,
            final Environment env) {
        if (exp instanceof SimpleVariable) {
            try {
                AbstractValue val = env.read(((SimpleVariable) exp).getName());
                if (val instanceof EventValue) {
                    return (EventValue) val;
                }
            } catch (InterpreterError ignored) {
                // Variable not found — not an event variable
            }
        }
        return null;
    }

    /**
     * Compare an event queue's front element with another value.
     * For ==: empty→false, peek matches→consume+true, no match→false.
     * For !=: empty→false, peek matches→false, no match→consume+true.
     * Consume happens only when the overall condition is true, following
     * the same pattern as TimeoutQuery (consumed when condition fires).
     */
    private BooleanValue evaluateEventCompare(
            final EventValue ev,
            final AbstractValue other,
            final boolean isEq) {
        if (ev.isEmpty()) {
            return new BooleanValue(false);
        }
        AbstractValue front = ev.peek();
        boolean matches = front.getType() == other.getType() && front.equalsValue(other);
        if (isEq) {
            if (matches) {
                ev.dequeue();
                return new BooleanValue(true);
            }
            return new BooleanValue(false);
        } else {
            // != : true when front does NOT match
            if (!matches) {
                ev.dequeue();
                return new BooleanValue(true);
            }
            return new BooleanValue(false);
        }
    }
}
