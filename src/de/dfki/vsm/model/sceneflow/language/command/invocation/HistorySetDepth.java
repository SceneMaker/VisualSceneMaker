package de.dfki.vsm.model.sceneflow.language.command.invocation;

import de.dfki.vsm.model.sceneflow.language.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class HistorySetDepth extends Invocation {

    // The state to set the history depth
    private String mState;
    // The depth to set the history to
    private int mDepth;

    public HistorySetDepth() {
    }

    public HistorySetDepth(final String state, final int depth) {
        mState = state;
        mDepth = depth;
    }

    public final int getDepth() {
        return mDepth;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "HistorySetDepth( " + mState + ", " + mDepth + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "HistorySetDepth( " + mState + ", " + mDepth + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#HistorySetDepth( " + "#c#" + mState + ", " + "#c#" + mDepth + ")";
    }

    @Override
    public final HistorySetDepth getCopy() {
        return new HistorySetDepth(mState, mDepth);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<HistorySetDepth state=\"" + mState + "\" depth=\"" + mDepth + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
