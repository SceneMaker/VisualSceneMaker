package de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression;

import de.dfki.vsm.util.ios.IndentWriter;
import org.w3c.dom.Element;

/**
 * A variable expression.
 *
 * @author Gregor Mehlmann
 */
public class VarExp extends LExpression {

    private String mName;

    public VarExp(String name) {
        mName = name;
    }

    public VarExp() {
        mName = new String();
    }

    public String getName() {
        return mName;
    }

    public LExpType getLExpType() {
        return LExpType.VARIABLE;
    }

    public String getAbstractSyntax() {
        return "VarExp(" + mName + ")";
    }

    public String getConcreteSyntax() {
        return mName;
    }

    public String getFormattedSyntax() {
        return mName;
    }

    public VarExp getCopy() {
        return new VarExp(mName);
    }

    public void writeXML(IndentWriter out) {
        out.println("<Variable name=\"" + mName + "\"/>");
    }

    public void parseXML(Element element) {
        mName = element.getAttribute("name");
    }
}
