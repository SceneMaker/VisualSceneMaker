package de.dfki.vsm.model.sceneflow.definition.type;

import de.dfki.vsm.model.sceneflow.Object;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * An abstract type definition.
 *
 * @author Gregor Mehlmann
 */
public abstract class TypeDef extends Object {

    public enum Flavour {

        List, Struct
    }
    protected String mName;

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
        String tag = element.getTagName();
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
