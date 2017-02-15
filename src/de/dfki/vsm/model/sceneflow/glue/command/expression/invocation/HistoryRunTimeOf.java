package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
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
        return "#p#HistoryRunTimeOf ( " + "#c#" + mNode + " , " + "#c#" + mDepth + " ) ";
    }

    @Override
    public HistoryRunTimeOf getCopy() {
        return new HistoryRunTimeOf(mNode, mDepth);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryRunTimeOf node=\"" + mNode + "\" depth=\"" + mDepth + "\"/>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mNode  = element.getAttribute("node");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
