package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class InStateQuery extends Expression {

    String mState;

    public InStateQuery() {
    }

    public InStateQuery(final String state) {
        mState = state;
    }

    public final String getState() {
        return mState;
    }

    @Override
    public final String getAbstractSyntax() {
        return "InStateQuery(" + mState + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "In (" + mState + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#In ( " + "#c#" + mState + " ) ";
    }

    @Override
    public final InStateQuery getCopy() {
        return new InStateQuery(mState);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<InStateQuery state=\"" + mState + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mState = element.getAttribute("state");
    }
}