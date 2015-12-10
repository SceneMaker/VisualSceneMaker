package de.dfki.vsm.model.sceneflow.language.command.expression.invocation;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class HistoryRunTime extends Expression {
    private String mNode;
    private int    mDepth;

    public HistoryRunTime() {}

    public HistoryRunTime(String node) {
        mNode  = node;
        mDepth = 0;
    }

    public HistoryRunTime(String node, int depth) {
        mNode  = node;
        mDepth = depth;
    }

    public String getNode() {
        return mNode;
    }

    public int getDepth() {
        return mDepth;
    }

//    @Override
//    public ExpType getExpType() {
//        return ExpType.HVO;
//    }

    @Override
    public String getAbstractSyntax() {
        return "HistoryRunTimeOf( " + mNode + " , " + mDepth + " )";
    }

    @Override
    public String getConcreteSyntax() {
        return "HistoryRunTimeOf( " + mNode + " , " + mDepth + " )";
    }

    @Override
    public String getFormattedSyntax() {
        return "#p#HistoryRunTimeOf( " + "#c#" + mNode + " , " + "#c#" + mDepth + " )";
    }

    public HistoryRunTime getCopy() {
        return new HistoryRunTime(mNode, mDepth);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryRunTimeOf node=\"" + mNode + "\" depth=\"" + mDepth + "\"/>").push();
    }

    public void parseXML(Element element) throws XMLParseError {
        mNode  = element.getAttribute("node");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
