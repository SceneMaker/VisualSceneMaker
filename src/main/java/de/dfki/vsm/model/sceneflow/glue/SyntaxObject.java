package de.dfki.vsm.model.sceneflow.glue;

import de.dfki.vsm.model.ModelObject;

/**
 * @author Gregor Mehlmann
 */
public abstract class SyntaxObject implements ModelObject {

    public abstract String getAbstractSyntax();

    public abstract String getConcreteSyntax();

    public abstract String getFormattedSyntax();

    @Override
    public final String toString() {
        return getConcreteSyntax();
    }
}