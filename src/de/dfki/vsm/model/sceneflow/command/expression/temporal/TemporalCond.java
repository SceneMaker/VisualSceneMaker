package de.dfki.vsm.model.sceneflow.command.expression.temporal;

//~--- non-JDK imports --------------------------------------------------------
//import de.dfki.vsm.model.sceneflow.command.expression.Condition;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression.ExpType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 *
 * @author Not me
 */
public abstract class TemporalCond extends AbstractExpression {

    public enum TemporalType {

        TIMEOUT
    }

    public abstract TemporalType getTemporalType();

    @Override
    public abstract TemporalCond getCopy();

    @Override
    public ExpType getExpType() {
        return ExpType.TEMP;
    }

    public static TemporalCond parse(Element element) throws XMLParseError {
        TemporalCond temp = null;
        String tag = element.getTagName();

        if (tag.equals("TimeoutCondition")) {
            temp = new TimeoutCond();
        } else {
            throw new XMLParseError(null,
                    "Cannot parse the element with the tag \"" + tag + "\" into a temporal condition!");
        }

        temp.parseXML(element);

        return temp;
    }
}
