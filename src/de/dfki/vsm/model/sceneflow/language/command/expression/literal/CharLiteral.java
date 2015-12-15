package de.dfki.vsm.model.sceneflow.language.command.expression.literal;

import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class CharLiteral extends LiteralExpression {
    
    private Character mValue;
    
    public CharLiteral() {
        mValue = null;
    }
    
    public CharLiteral(Character value) {
        mValue = value;
    }
    
    public Character getValue() {
        return mValue;
    }
    
    public void setValue(Character value) {
        mValue = value;
    }
    
    @Override
    public ConstType getConstType() {
        return ConstType.STRING;
    }
    
    @Override
    public String getAbstractSyntax() {
        return "String(" + getConcreteSyntax() + ")";
    }
    
    @Override
    public String getConcreteSyntax() {
        return "\"" + mValue + "\"";
    }
    
    @Override
    public String getFormattedSyntax() {
        return TextFormat.formatConstantStringLiteral("\"" + mValue + "\"");
    }
    
    @Override
    public CharLiteral getCopy() {
        return new CharLiteral(mValue);
    }
    
    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<String value=\"" + mValue + "\"/>");
    }
    
    @Override
    public void parseXML(Element element) {
        mValue = element.getAttribute("value").charAt(0);
    }
}
