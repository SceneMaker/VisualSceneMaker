package de.dfki.vsm.model.sceneflow.glue.command.expression;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.NullLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmannn
 */
public abstract class LiteralExpression extends Expression {

    @Override
    public abstract LiteralExpression getCopy();

    public static LiteralExpression parse(final Element element) throws XMLParseError {
        LiteralExpression literal;
        final String tag = element.getTagName();
        if (tag.equals("IntLiteral")) {
            literal = new IntLiteral();
            literal.parseXML(element);
        } else if (tag.equals("FloatLiteral")) {
            literal = new FloatLiteral();
            literal.parseXML(element);
        } else if (tag.equals("BoolLiteral")) {
            literal = new BoolLiteral();
            literal.parseXML(element);
        } else if (tag.equals("StringLiteral")) {
            literal = new StringLiteral();
            literal.parseXML(element);
        } else if (tag.equals("NullLiteral")) {
            literal = new NullLiteral();
            literal.parseXML(element);
        } else {
            literal = null;
        }
        return literal;
    }
}
