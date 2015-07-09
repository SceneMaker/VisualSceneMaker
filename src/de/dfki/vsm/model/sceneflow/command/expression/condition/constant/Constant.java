package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition.CondType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * A constant expression.
 *
 * @author Not men
 */
public abstract class Constant extends Condition {
    public enum ConstType {
        BOOL, INT, STRING, FLOAT, LIST, STRUCT, OBJECT
    }

    public CondType getCondType() {
        return CondType.CONST;
    }

    public abstract ConstType getConstType();

    public abstract Constant getCopy();

    public static Constant parse(Element element) throws XMLParseError {
        Constant         cons = null;
        java.lang.String tag  = element.getTagName();

        if (tag.equals("Int")) {
            cons = new Int();
        } else if (tag.equals("Float")) {
            cons = new Float();
        } else if (tag.equals("Bool")) {
            cons = new Bool();
        } else if (tag.equals("String")) {
            cons = new String();
        } else if (tag.equals("List")) {
            cons = new List();
        } else if (tag.equals("Struct")) {
            cons = new Struct();
        } else if (tag.equals("Object")) {
            cons = new Object();
        } else {    /* Error */
        }

        cons.parseXML(element);

        return cons;
    }
}
