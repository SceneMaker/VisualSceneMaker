package de.dfki.vsm.model.sceneflow.language.command.invocation;

import de.dfki.vsm.model.sceneflow.language.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class HistoryDeepClear extends Invocation {

    // The state to clear the history
    private String mState;

    public HistoryDeepClear() {
    }

    public HistoryDeepClear(final String state) {
        mState = state;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "HistoryDeepClear( " + mState + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "HistoryDeepClear( " + mState + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#HistoryDeepClear( " + "#c#" + mState + ")";
    }

    @Override
    public final HistoryDeepClear getCopy() {
        return new HistoryDeepClear(mState);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<HistoryDeepClear state=\"" + mState + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
    }
}
