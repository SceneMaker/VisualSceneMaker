package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class HistoryRunTimeOf extends Expression {
    private String mNode;
    private int    mDepth;

    public HistoryRunTimeOf() {}

    public HistoryRunTimeOf(String node) {
        mNode  = node;
        mDepth = 0;
    }

    public HistoryRunTimeOf(String node, int depth) {
        mNode  = node;
        mDepth = depth;
    }

    public String getNode() {
        return mNode;
    }

    public int getDepth() {
        return mDepth;
    }

    public ExpType getExpType() {
        return ExpType.HVO;
    }

    public String getAbstractSyntax() {
        return "HistoryRunTimeOf( " + mNode + " , " + mDepth + " )";
    }

    public String getConcreteSyntax() {
        return "HistoryRunTimeOf( " + mNode + " , " + mDepth + " )";
    }

    public String getFormattedSyntax() {
        return "#p#HistoryRunTimeOf( " + "#c#" + mNode + " , " + "#c#" + mDepth + " )";
    }

    public HistoryRunTimeOf getCopy() {
        return new HistoryRunTimeOf(mNode, mDepth);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryRunTimeOf node=\"" + mNode + "\" depth=\"" + mDepth + "\"/>").push();
    }

    public void parseXML(Element element) throws XMLParseError {
        mNode  = element.getAttribute("node");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
