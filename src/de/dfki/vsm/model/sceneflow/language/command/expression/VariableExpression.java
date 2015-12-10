package de.dfki.vsm.model.sceneflow.language.command.expression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.ArrayVariable;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.MemberVariable;
import de.dfki.vsm.model.sceneflow.language.command.expression.variable.SimpleVariable;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * An abstract left side expression.
 *
 * @author Not me
 */
public abstract class VariableExpression extends Expression {

    public enum LExpType {

        MEMBER, VARIABLE, ARRAY
    }

    public abstract LExpType getLExpType();

    @Override
    public abstract VariableExpression getCopy();

//    @Override
//    public ExpType getExpType() {
//        return ExpType.LEXP;
//    }

    public static VariableExpression parse(Element element) throws XMLParseError {
        VariableExpression log = null;
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
