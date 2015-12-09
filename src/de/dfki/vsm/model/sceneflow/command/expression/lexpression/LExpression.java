package de.dfki.vsm.model.sceneflow.command.expression.lexpression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.Expression.ExpType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * An abstract left side expression.
 *
 * @author Not me
 */
public abstract class LExpression extends Expression {

    public enum LExpType {

        MEMBER, VARIABLE, ARRAY
    }

    public abstract LExpType getLExpType();

    @Override
    public abstract LExpression getCopy();

    @Override
    public ExpType getExpType() {
        return ExpType.LEXP;
    }

    public static LExpression parse(Element element) throws XMLParseError {
        LExpression log = null;
        String tag = element.getTagName();

        if (tag.equals("Variable")) {
            log = new SimpleVariable();
        } else if (tag.equals("Member")) {
            log = new MemberVariable();
        } else if (tag.equals("Field")) {
            log = new ArrayVariable();
        } else {

            /* Error */
        }

        log.parseXML(element);

        return log;
    }
}
