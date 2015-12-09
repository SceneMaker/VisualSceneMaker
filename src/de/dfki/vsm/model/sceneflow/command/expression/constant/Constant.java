package de.dfki.vsm.model.sceneflow.command.expression.constant;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression.ExpType;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * A constant expression.
 *
 * @author Not men
 */
public abstract class Constant extends AbstractExpression {

    public enum ConstType {

        BOOL, INT, STRING, FLOAT, LIST, STRUCT, OBJECT
    }

    /**
     *
     * @return
     */
    @Override
    public ExpType getExpType() {
        return ExpType.CONST;
    }

    public abstract ConstType getConstType();

    @Override
    public abstract Constant getCopy();

    public static Constant parse(Element element) throws XMLParseError {
        Constant cons = null;
        java.lang.String tag = element.getTagName();

        if (tag.equals("Int")) {
            cons = new IntLiteral();
        } else if (tag.equals("Float")) {
            cons = new FloatLiteral();
        } else if (tag.equals("Bool")) {
            cons = new BoolLiteral();
        } else if (tag.equals("String")) {
            cons = new StringLiteral();
        } else if (tag.equals("List")) {
            cons = new ListRecord();
        } else if (tag.equals("Struct")) {
            cons = new StructRecord();
        } else if (tag.equals("Object")) {
            cons = new JavaObject();
        } else {    /* Error */

        }
        cons.parseXML(element);
        return cons;
    }
}
