package de.dfki.vsm.model.sceneflow;

import de.dfki.vsm.model.ModelObject;

/**
 * @author Gregor Mehlmann
 */
public interface SyntaxObject extends ModelObject {

    public abstract String getAbstractSyntax();

    public abstract String getConcreteSyntax();

    public abstract String getFormattedSyntax();

   
}
