package de.dfki.vsm.model.sceneflow.definition.type;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * An abstract type definition.
 *
 * @author Not me
 */
public abstract class TypeDef extends Syntax {
    protected String mName;

    public enum Flavour { List, Struct }

    public TypeDef() {
        mName = new String();
    }

    public TypeDef(String name) {
        mName = name;
    }

    public void setName(String value) {
        mName = value;
    }

    public String getName() {
        return mName;
    }

    public abstract TypeDef getCopy();

    public abstract Flavour getFlavour();

    public static TypeDef parse(Element element) throws XMLParseError {
        TypeDef type = null;
        String  tag  = element.getTagName();

        if (tag.equals("ListType")) {
            type = new ListTypeDef();
        } else if (tag.equals("StructType")) {
            type = new StructTypeDef();
        } else {
            throw new XMLParseError(null, "Hilfe!");
        }

        type.parseXML(element);

        return type;
    }
}
