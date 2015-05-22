package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.cpy.Copyable;

/**
 * @author Gregor Mehlmann
 */
public abstract class Object implements ModelObject, Copyable {
    public abstract String getAbstractSyntax();

    public abstract String getConcreteSyntax();

    public abstract String getFormattedSyntax();

    @Override
    public String toString() {
        return getConcreteSyntax();
    }
}
