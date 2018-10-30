package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class HistoryContains extends Expression {

    String mState;
    String mSubState;
    int mDepth;

    public HistoryContains() {
    }

    public HistoryContains(String state, String subState) {
        mState = state;
        mSubState = subState;
        mDepth = 0;
    }

    public HistoryContains(String state, String subState, int depth) {
        mState = state;
        mSubState = subState;
        mDepth = depth;
    }

    public String getState() {
        return mState;
    }

    public String getSubState() {
        return mSubState;
    }

    public int getDepth() {
        return mDepth;
    }

    @Override
    public String getAbstractSyntax() {
        return "HistoryContains(" + mState + "," + mSubState + "," + mDepth + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return "HistoryContains(" + mState + "," + mSubState + "," + mDepth + ")";
    }

    @Override
    public String getFormattedSyntax() {
        return "#p#HistoryContains ( " + mState + "," + mSubState + "," + mDepth + " ) ";
    }

    @Override
    public HistoryContains getCopy() {
        return new HistoryContains(mState, mSubState, mDepth);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryContains state=\"" + mState + "\" substate=\"" + mSubState + "\" depth=\"" + mDepth + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        mState = element.getAttribute("state");
        mSubState = element.getAttribute("substate");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
