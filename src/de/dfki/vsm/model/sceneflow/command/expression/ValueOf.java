package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression.ExpType;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class ValueOf extends Expression {
    private String mNode;
    private String mVariable;

    public ValueOf() {
        mNode     = null;
        mVariable = null;
    }

    public ValueOf(String node, String variable) {
        mNode     = node;
        mVariable = variable;
    }

    public String getNode() {
        return mNode;
    }

    public String getVar() {
        return mVariable;
    }

    public ExpType getExpType() {
        return ExpType.VO;
    }

    public String getAbstractSyntax() {
        return "ValueOf( " + mNode + " , " + mVariable + " )";
    }

    public String getConcreteSyntax() {
        return "ValueOf( " + mNode + " , " + mVariable + " )";
    }

    public String getFormattedSyntax() {
        return "#p#ValueOf( " + "#c#" + mNode + " , " + mVariable + " )";
    }

    public ValueOf getCopy() {
        return new ValueOf(mNode, mVariable);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<ValueOf node=\"" + mNode + "\" var=\"" + mVariable    /* + "\" depth=\"" + mDepth */
                    + "\"/>").push();
    }

    public void parseXML(Element element) throws XMLParseError {
        mNode     = element.getAttribute("node");
        mVariable = element.getAttribute("var");

        // mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
