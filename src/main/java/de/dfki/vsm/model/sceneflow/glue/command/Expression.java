package de.dfki.vsm.model.sceneflow.glue.command;

import de.dfki.vsm.model.sceneflow.glue.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ConstructExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryRunTimeOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.glue.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.ParenExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.ContainsList;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryContains;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.InStateQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.PrologQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.RandomQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.TimeoutQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Expression extends Command {

    @Override
    public abstract Expression getCopy();

    public static Expression parse(final Element element) throws XMLParseError {
        // The expression to parse
        Expression exp;
        // The name of the XML tag
        final String tag = element.getTagName();
        // Parse the expression
        switch (tag) {
            case "CallingExpression":
                exp = new CallingExpression();
                exp.parseXML(element);
                break;
            case "ConstructExpression":
                exp = new ConstructExpression();
                exp.parseXML(element);
                break;
            case "Neg":
            case "Not":
            case "Lnot":
            case "Inc":
            case "Dec":
                exp = new UnaryExpression();
                exp.parseXML(element);
                break;
            case "AndAnd":
            case "OrOr":
            case "And":
            case "Or":
            case "Xor":
            case "Add":
            case "Div":
            case "Mul":
            case "Mod":
            case "Sub":
            case "Eq":
            case "Ge":
            case "Gt":
            case "Le":
            case "Lt":
            case "Neq":
                exp = new BinaryExpression();
                exp.parseXML(element);
                break;
            case "TernaryExpression":
                exp = new TernaryExpression();
                exp.parseXML(element);
                break;
            case "ParenExpression":
                exp = new ParenExpression();
                exp.parseXML(element);
                break;
            case "HistoryValueOf":
                exp = new HistoryValueOf();
                exp.parseXML(element);
                break;
            case "HistoryRunTimeOf":
                exp = new HistoryRunTimeOf();
                exp.parseXML(element);
                break;
            case "HistoryContains":
                exp = new HistoryContains();
                exp.parseXML(element);
                break;
            case "InStateQuery":
                exp = new InStateQuery();
                exp.parseXML(element);
                break;
            case "PrologQuery":
                exp = new PrologQuery();
                exp.parseXML(element);
                break;
            case "TimeoutQuery":
                exp = new TimeoutQuery();
                exp.parseXML(element);
                break;
            case "RandomQuery":
                exp = new RandomQuery();
                exp.parseXML(element);
                break;
            case "ContainsList":
                exp = new ContainsList();
                exp.parseXML(element);
                System.err.println(exp);
                break;
            case "StructExpression":
                exp = new StructExpression();
                exp.parseXML(element);
                break;
            case "ArrayExpression":
                exp = new ArrayExpression();
                exp.parseXML(element);
                break;
            case "IntLiteral":
            case "FloatLiteral":
            case "BoolLiteral":
            case "StringLiteral":
            case "NullLiteral":
                exp = LiteralExpression.parse(element);
                break;
            case "SimpleVariable":
            case "MemberVariable":
            case "FieldVariable":
                exp = VariableExpression.parse(element);
                break;
            default:
                exp = null;
                break;
        }
        return exp;
    }
}
