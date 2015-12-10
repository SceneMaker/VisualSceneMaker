package de.dfki.vsm.model.sceneflow.language;

import de.dfki.vsm.model.ModelObject;

/**
 * @author Gregor Mehlmann
 */
public abstract class LanguageObject implements ModelObject {

    // Get The Abstract Syntax Of The Object
    public abstract String getAbstractSyntax();

    // Get The Concrete Syntax Of The Object
    public abstract String getConcreteSyntax();

    // Get The Formatted Syntax Of The Object
    public abstract String getFormattedSyntax();

    // Get The String Syntax Representation 
    @Override
    public String toString() {
        return getAbstractSyntax();
        //return getConcreteSyntax();
    }
}
