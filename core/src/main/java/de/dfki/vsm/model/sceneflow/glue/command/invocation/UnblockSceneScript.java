package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class UnblockSceneScript extends Invocation {

    private boolean mNonAbstract;

    public UnblockSceneScript() {
        mNonAbstract = false;
    }

    public UnblockSceneScript(final boolean value) {
        mNonAbstract = value;
    }

    public final void setNonAbstract(final boolean value) {
        mNonAbstract = value;
    }

    public final boolean getNonAbstract() {
        return mNonAbstract;
    }

    @Override
    public final String getAbstractSyntax() {
        return "UnblockSceneScript(" + mNonAbstract + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "UnblockSceneScript(" + mNonAbstract + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#UnblockSceneScript ( " + "#r#" + mNonAbstract + " ) ";
    }

    @Override
    public final UnblockSceneScript getCopy() {
        return new UnblockSceneScript(mNonAbstract);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<UnblockSceneScript non-abstract=\"" + mNonAbstract + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mNonAbstract = Boolean.valueOf(element.getAttribute("non-abstract"));
    }
}