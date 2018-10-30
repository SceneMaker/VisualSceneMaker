package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class HistoryClearDeep extends Invocation {

    private String mState;

    public HistoryClearDeep() {
    }

    public HistoryClearDeep(final String state) {
        mState = state;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "HistoryClearDeep(" + mState + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "HistoryClearDeep(" + mState + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#HistoryClearDeep(" + "#c#" + mState + ")";
    }

    @Override
    public final HistoryClearDeep getCopy() {
        return new HistoryClearDeep(mState);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<HistoryClearDeep state=\"" + mState + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
    }
}
