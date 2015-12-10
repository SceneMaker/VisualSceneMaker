package de.dfki.vsm.model.sceneflow.language.command;

import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.HistoryContains;
import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.HistoryRunTime;
import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.InStateCond;
import de.dfki.vsm.model.sceneflow.language.command.expression.BinaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.CallingExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.ConstructExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.ParenthesesExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.TernaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.PrologQuery;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.ListRecord;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.StructRecord;
import de.dfki.vsm.model.sceneflow.language.command.expression.invocation.TimeoutFunction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Expression extends Command {

    public static Expression parse(final Element element) throws XMLParseError {
        // The expression to parse
        Expression expression;
        // The name of the XML tag
        final String tag = element.getTagName();

        if (tag.equals("UserCommand")) {
            expression = new CallingExpression();
            expression.parseXML(element);
        } else if (tag.equals("If")) {
            expression = new TernaryExpression();
            expression.parseXML(element);
        } else if (tag.equals("Constructor")) {
            expression = new ConstructExpression();
            expression.parseXML(element);
        } else if (tag.equals("Parentheses")) {
            expression = new ParenthesesExpression();
            expression.parseXML(element);
        } else if (tag.equals("Random")
                || tag.equals("First")
                || tag.equals("Last")
                || tag.equals("Clear")
                || tag.equals("Size")
                || tag.equals("RemoveFirst")
                || tag.equals("RemoveLast")
                || tag.equals("Neg")
                || tag.equals("Not")) {
            expression = new UnaryExpression();
            expression.parseXML(element);
        } else if (tag.equals("Add")
                || tag.equals("Div")
                || tag.equals("Mul")
                || tag.equals("Sub")
                || tag.equals("Get")
                || tag.equals("Remove")
                || tag.equals("AddFirst")
                || tag.equals("AddLast")
                || tag.equals("And")
                || tag.equals("Or")
                || tag.equals("Eq")
                || tag.equals("Ge")
                || tag.equals("Gt")
                || tag.equals("Le")
                || tag.equals("Lt")
                || tag.equals("Neq")) {
            expression = new BinaryExpression();
            expression.parseXML(element);
        } else if (tag.equals("HistoryValueOf")) {
            expression = new HistoryValueOf();
            expression.parseXML(element);
        } else if (tag.equals("HistoryRunTimeOf")) {
            expression = new HistoryRunTime();
            expression.parseXML(element);
        } else if (tag.equals("HistoryContainsState")) {
            expression = new HistoryContains();
            expression.parseXML(element);
        } else if (tag.equals("StateCondition")) {
            expression = new InStateCond();
            expression.parseXML(element);
        } else if (tag.equals("PrologCondition")) {
            expression = new PrologQuery();
            expression.parseXML(element);
        } else if (tag.equals("Member")
                || tag.equals("Variable")
                || tag.equals("Field")) {
            expression = VariableExpression.parse(element);
            expression.parseXML(element);
        } else if (tag.equals("Int")
                || tag.equals("Float")
                || tag.equals("Object")
                || tag.equals("Bool")
                || tag.equals("String")
                || tag.equals("List")
                || tag.equals("Struct")) {
            expression = LiteralExpression.parse(element);
            expression.parseXML(element);
        } else if (tag.equals("TimeoutCondition")) {
            expression = new TimeoutFunction();
            expression.parseXML(element);
        } else if (tag.equals("List")) {
            expression = new ListRecord();
            expression.parseXML(element);
        } else if (tag.equals("Struct")) {
            expression = new StructRecord();
            expression.parseXML(element);
        } else {
            // Do nothing and return with NULL
            expression = null;
        }
        return expression;
    }

    @Override
    public abstract Expression getCopy();
}
