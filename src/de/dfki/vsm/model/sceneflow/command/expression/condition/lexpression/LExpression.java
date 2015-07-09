package de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition.CondType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * An abstract left side expression.
 *
 * @author Not me
 */
public abstract class LExpression extends Condition {
    public enum LExpType { MEMBER, VARIABLE, ARRAY }

    public abstract LExpType getLExpType();

    public abstract LExpression getCopy();

    public CondType getCondType() {
        return CondType.LEXP;
    }

    public static LExpression parse(Element element) throws XMLParseError {
        LExpression log = null;
        String      tag = element.getTagName();

        if (tag.equals("Variable")) {
            log = new VarExp();
        } else if (tag.equals("Member")) {
            log = new MemVarExp();
        } else if (tag.equals("Field")) {
            log = new ArrVarExp();
        } else {

            /* Error */
        }

        log.parseXML(element);

        return log;
    }
}
