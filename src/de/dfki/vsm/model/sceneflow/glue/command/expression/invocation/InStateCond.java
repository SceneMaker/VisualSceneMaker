package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class InStateCond extends Expression {

    String mState;

    public InStateCond() {
    }

    public InStateCond(final String state) {
        mState = state;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "InStateCond(" + mState + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "InStateCond(" + mState + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#InStateCond ( " + "#c#" + mState + " ) ";
    }

    @Override
    public final InStateCond getCopy() {
        return new InStateCond(mState);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<InStateCond state=\"" + mState + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
    }
}