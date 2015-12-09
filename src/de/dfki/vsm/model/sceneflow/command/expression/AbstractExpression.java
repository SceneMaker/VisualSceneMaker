package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.expression.function.HistoryContains;
import de.dfki.vsm.model.sceneflow.command.expression.function.HistoryRunTime;
import de.dfki.vsm.model.sceneflow.command.expression.function.HistoryValueOf;
import de.dfki.vsm.model.sceneflow.command.expression.function.StateValueOf;
import de.dfki.vsm.model.sceneflow.command.expression.function.InStateCond;
import de.dfki.vsm.model.sceneflow.command.AbstractCommand;
import de.dfki.vsm.model.sceneflow.command.AbstractCommand.CmdType;
import de.dfki.vsm.model.sceneflow.command.expression.lexpression.AbstractVariable;
import de.dfki.vsm.model.sceneflow.command.expression.constant.Constant;
import de.dfki.vsm.model.sceneflow.command.expression.function.PrologQuery;
import de.dfki.vsm.model.sceneflow.command.expression.temporal.TemporalCond;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public abstract class AbstractExpression extends AbstractCommand {

    // TODO: Rename the expression types
    public enum ExpType {

        UNARYEXP,
        BINARYEXP,
        CONDITIONAL,
        PARENEXP,
        HVO, HCS, USR, VO, CONS, STATE,
        PROLOG_QUERY, CONST, LEXP, TEMP
    }

    public abstract ExpType getExpType();

    @Override
    public abstract AbstractExpression getCopy();

    @Override
    public CmdType getCmdType() {
        return CmdType.EXP;
    }

    public static AbstractExpression parse(Element element) throws XMLParseError {
        AbstractExpression exp = null;
        String tag = element.getTagName();

        if (tag.equals("UserCommand")) {
            exp = new CallingExpression();
            exp.parseXML(element);
        } else if (tag.equals("If")) {
            exp = new TernaryExpression();
            exp.parseXML(element);
        } else if (tag.equals("Constructor")) {
            exp = new ConstructExpression();
            exp.parseXML(element);
        } else if (tag.equals("Parentheses")) {
            exp = new ParenthesesExpression();
            exp.parseXML(element);
        } else if (tag.equals("Random")
                || tag.equals("First")
                || tag.equals("Last")
                || tag.equals("Clear")
                || tag.equals("Size")
                || tag.equals("RemoveFirst")
                || tag.equals("RemoveLast")
                || tag.equals("Neg")
                || tag.equals("Not")) {
            exp = new UnaryExpression();
            exp.parseXML(element);
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
            exp = new BinaryExpression();
            exp.parseXML(element);
        } else if (tag.equals("HistoryValueOf")) {
            exp = new HistoryValueOf();
            exp.parseXML(element);
        } else if (tag.equals("HistoryRunTimeOf")) {
            exp = new HistoryRunTime();
            exp.parseXML(element);
        } else if (tag.equals("HistoryContainsState")) {
            exp = new HistoryContains();
            exp.parseXML(element);
        } else if (tag.equals("StateCondition")) {
            exp = new InStateCond();
            exp.parseXML(element);
        } else if (tag.equals("ValueOf")) {
            exp = new StateValueOf();
            exp.parseXML(element);
        } else if (tag.equals("PrologCondition")) {
            exp = new PrologQuery();
            exp.parseXML(element);
        } else if (tag.equals("Member")
                || tag.equals("Variable")
                || tag.equals("Field")) {
            exp = AbstractVariable.parse(element);
            exp.parseXML(element);
        } else if (tag.equals("Int")
                || tag.equals("Float")
                || tag.equals("Object")
                || tag.equals("Bool")
                || tag.equals("String")
                || tag.equals("List")
                || tag.equals("Struct")) {
            exp = Constant.parse(element);
            exp.parseXML(element);
        } else if (tag.equals("TimeoutCondition")) {
            exp = TemporalCond.parse(element);
            exp.parseXML(element);
        } else {
            // Error
        }

        return exp;
    }
}
