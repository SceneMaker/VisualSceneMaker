package de.dfki.vsm.model.sceneflow.language.command.invocation;

import de.dfki.vsm.model.sceneflow.language.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class HistoryFlatClear extends Invocation {

     // The state to clear the history
    private String mState;

    public HistoryFlatClear() {
    }

    public HistoryFlatClear(final String state) {
        mState = state;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "HistoryFlatClear( " + mState + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "HistoryFlatClear( " + mState + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#HistoryFlatClear( " + "#c#" + mState + ")";
    }

    @Override
    public final HistoryFlatClear getCopy() {
        return new HistoryFlatClear(mState);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<HistoryFlatClear state=\"" + mState + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
    }
}
