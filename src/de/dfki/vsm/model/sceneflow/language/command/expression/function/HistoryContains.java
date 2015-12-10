package de.dfki.vsm.model.sceneflow.language.command.expression.function;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 *
 * @author Not me
 */
public class HistoryContains extends Expression{
    String mState;
    String mSubState;
    int    mDepth;

    public HistoryContains() {}

    public HistoryContains(String state, String subState) {
        mState    = state;
        mSubState = subState;
        mDepth    = 0;
    }

    public HistoryContains(String state, String subState, int depth) {
        mState    = state;
        mSubState = subState;
        mDepth    = depth;
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
//
//    @Override
//   public ExpType getExpType() {
//        return ExpType.HCS;
//    }

    @Override
    public String getAbstractSyntax() {
        return "HistoryContainsState(" + mState + "," + mSubState + "," + mDepth + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return "HistoryContainsState(" + mState + "," + mSubState + "," + mDepth + ")";
    }

    @Override
    public String getFormattedSyntax() {
        return "";
    }

    @Override
    public HistoryContains getCopy() {
        return new HistoryContains(mState, mSubState, mDepth);
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryContainsState state=\"" + mState + "\" substate=\"" + mSubState + "\" depth=\"" + mDepth
                    + "\"/>");
    }

    @Override
    public void parseXML(Element element) {
        mState    = element.getAttribute("state");
        mSubState = element.getAttribute("substate");
        mDepth    = Integer.valueOf(element.getAttribute("depth"));
    }
}
