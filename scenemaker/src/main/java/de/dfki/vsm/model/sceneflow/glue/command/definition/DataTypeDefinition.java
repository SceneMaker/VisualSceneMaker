package de.dfki.vsm.model.sceneflow.glue.command.definition;

import de.dfki.vsm.model.sceneflow.glue.command.Definition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.ListTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.StructTypeDefinition;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class DataTypeDefinition extends Definition {

    protected String mName;

    public final void setName(final String name) {
        mName = name;
    }

    public final String getName() {
        return mName;
    }

    public enum Flavour {
        List, Struct
    };

    public abstract Flavour getFlavour();

    @Override
    public abstract DataTypeDefinition getCopy();

    public static DataTypeDefinition parse(final Element element) throws XMLParseError {
        DataTypeDefinition def;
        final String tag = element.getTagName();
        if (tag.equals("ListTypeDefinition")) {
            def = new ListTypeDefinition();
            def.parseXML(element);
        } else if (tag.equals("StructTypeDefinition")) {
            def = new StructTypeDefinition();
            def.parseXML(element);
        } else {
            def = null;
        }
        return def;
    }
}