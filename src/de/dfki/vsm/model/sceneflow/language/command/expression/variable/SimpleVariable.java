package de.dfki.vsm.model.sceneflow.language.command.expression.variable;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.sceneflow.language.command.expression.VariableExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A variable expression.
 *
 * @author Gregor Mehlmann
 */
public class SimpleVariable extends VariableExpression {

    private String mName;

    public SimpleVariable() {
        mName = new String();
    }

    public SimpleVariable(String name) {
        mName = name;
        
        System.err.println("Creating Variable " + name);
    }

    public String getName() {
        return mName;
    }

    @Override
    public LExpType getLExpType() {
        return LExpType.VARIABLE;
    }

    @Override
    public String getAbstractSyntax() {
        return "VarExp(" + mName + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return mName;
    }

    @Override
    public String getFormattedSyntax() {
        return mName;
    }

    @Override
    public SimpleVariable getCopy() {
        return new SimpleVariable(mName);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Variable name=\"" + mName + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        mName = element.getAttribute("name");
    }
}
