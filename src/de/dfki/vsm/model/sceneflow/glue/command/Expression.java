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
        if (tag.equals("CallingExpression")) {
            exp = new CallingExpression();
            exp.parseXML(element);
        } else if (tag.equals("ConstructExpression")) {
            exp = new ConstructExpression();
            exp.parseXML(element);
        } else if (tag.equals("Neg")
                || tag.equals("Not")
                || tag.equals("Lnot")
                || tag.equals("Inc")
                || tag.equals("Dec")) {
            exp = new UnaryExpression();
            exp.parseXML(element);
        } else if (tag.equals("AndAnd")
                || tag.equals("OrOr")
                || tag.equals("And")
                || tag.equals("Or")
                || tag.equals("Xor")
                || tag.equals("Add")
                || tag.equals("Div")
                || tag.equals("Mul")
                || tag.equals("Mod")
                || tag.equals("Sub")
                || tag.equals("Eq")
                || tag.equals("Ge")
                || tag.equals("Gt")
                || tag.equals("Le")
                || tag.equals("Lt")
                || tag.equals("Neq")) {
            exp = new BinaryExpression();
            exp.parseXML(element);
        } else if (tag.equals("TernaryExpression")) {
            exp = new TernaryExpression();
            exp.parseXML(element);
        } else if (tag.equals("ParenExpression")) {
            exp = new ParenExpression();
            exp.parseXML(element);
        } else if (tag.equals("HistoryValueOf")) {
            exp = new HistoryValueOf();
            exp.parseXML(element);
        } else if (tag.equals("HistoryRunTimeOf")) {
            exp = new HistoryRunTimeOf();
            exp.parseXML(element);
        } else if (tag.equals("HistoryContains")) {
            exp = new HistoryContains();
            exp.parseXML(element);
        } else if (tag.equals("InStateQuery")) {
            exp = new InStateQuery();
            exp.parseXML(element);
        } else if (tag.equals("PrologQuery")) {
            exp = new PrologQuery();
            exp.parseXML(element);
        } else if (tag.equals("TimeoutQuery")) {
            exp = new TimeoutQuery();
            exp.parseXML(element);
        } else if (tag.equals("RandomQuery")) {
            exp = new RandomQuery();
            exp.parseXML(element);
        } else if (tag.equals("ContainsList")) {
            exp = new ContainsList();
            exp.parseXML(element);
            System.err.println(exp);
        } else if (tag.equals("StructExpression")) {
            exp = new StructExpression();
            exp.parseXML(element);
        } else if (tag.equals("ArrayExpression")) {
            exp = new ArrayExpression();
            exp.parseXML(element);
        } else if (tag.equals("IntLiteral")
                || tag.equals("FloatLiteral")
                || tag.equals("BoolLiteral")
                || tag.equals("StringLiteral")
                || tag.equals("NullLiteral")) {
            exp = LiteralExpression.parse(element);
        } else if (tag.equals("SimpleVariable")
                || tag.equals("MemberVariable")
                || tag.equals("FieldVariable")) {
            exp = VariableExpression.parse(element);
        } else {
            exp = null;
        }
        return exp;
    }
}
