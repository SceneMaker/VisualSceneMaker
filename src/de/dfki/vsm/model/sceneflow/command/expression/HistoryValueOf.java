package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class HistoryValueOf extends Expression {
    private String mNode;
    private String mVariable;
    private int    mDepth;

    public HistoryValueOf() {}

    public HistoryValueOf(String node, String variable) {
        mNode     = node;
        mVariable = variable;
        mDepth    = 0;
    }

    public HistoryValueOf(String node, String variable, int depth) {
        mNode     = node;
        mVariable = variable;
        mDepth    = depth;
    }

    public String getNode() {
        return mNode;
    }

    public String getVar() {
        return mVariable;
    }

    public int getDepth() {
        return mDepth;
    }

    public ExpType getExpType() {
        return ExpType.HVO;
    }

    public String getAbstractSyntax() {
        return "HistoryValueOf( " + mNode + " , " + mVariable + " , " + mDepth + " )";
    }

    public String getConcreteSyntax() {
        return "HistoryValueOf( " + mNode + " , " + mVariable + " , " + mDepth + " )";
    }

    public String getFormattedSyntax() {
        return "#p#HistoryValueOf( " + "#c#" + mNode + " , " + mVariable + " , " + "#c#" + mDepth + " )";
    }

    public HistoryValueOf getCopy() {
        return new HistoryValueOf(mNode, mVariable, mDepth);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryValueOf node=\"" + mNode + "\" var=\"" + mVariable + "\" depth=\"" + mDepth
                    + "\"/>").push();
    }

    public void parseXML(Element element) throws XMLParseError {
        mNode     = element.getAttribute("node");
        mVariable = element.getAttribute("var");
        mDepth    = Integer.valueOf(element.getAttribute("depth"));
    }
}
