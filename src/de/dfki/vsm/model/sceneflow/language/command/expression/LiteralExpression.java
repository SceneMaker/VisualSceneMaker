package de.dfki.vsm.model.sceneflow.language.command.expression;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.NullObject;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.StringLiteral;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * A constant expression.
 *
 * @author Not men
 */
public abstract class LiteralExpression extends Expression {

    public enum ConstType {

        INT, SHORT, LONG, BOOL, STRING, FLOAT, DOUBLE, NULL
    }

//    /**
//     *
//     * @return
//     */
//    @Override
//    public ExpType getExpType() {
//        return ExpType.CONST;
//    }
    public abstract ConstType getConstType();

    @Override
    public abstract LiteralExpression getCopy();

    public static LiteralExpression parse(Element element) throws XMLParseError {
        LiteralExpression cons = null;
        java.lang.String tag = element.getTagName();

        if (tag.equals("Int")) {
            cons = new IntLiteral();
        } else if (tag.equals("Float")) {
            cons = new FloatLiteral();
        } else if (tag.equals("Bool")) {
            cons = new BoolLiteral();
        } else if (tag.equals("String")) {
            cons = new StringLiteral();
        } else if (tag.equals("Null")) {
            cons = new NullObject();
        } else {    /* Error */

        }
        cons.parseXML(element);
        return cons;
    }
}
