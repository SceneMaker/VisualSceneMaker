package de.dfki.vsm.model.sceneflow.command.expression.condition.temporal;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition.CondType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 *
 * @author Not me
 */
public abstract class TemporalCond extends Condition {
    public enum TemporalType { TIMEOUT }

    public abstract TemporalType getTemporalType();

    public abstract TemporalCond getCopy();

    public CondType getCondType() {
        return CondType.TEMP;
    }

    public static TemporalCond parse(Element element) throws XMLParseError {
        TemporalCond temp = null;
        String       tag  = element.getTagName();

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
