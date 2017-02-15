package de.dfki.vsm.model.sceneflow.glue.command.definition.datatype;

import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ListTypeDefinition extends DataTypeDefinition {

    //private String mName;
    private String mType;

    public ListTypeDefinition() {
        mName = null;
        mType = null;
    }

    public ListTypeDefinition(final String name, final String type) {
        mName = name;
        mType = type;
    }
    
    @Override
     public Flavour getFlavour() {return Flavour.List; }

//    public final void setName(final String name) {
//        mName = name;
//    }
//
//    public final String getName() {
//        return mName;
//    }

    public final void setType(final String type) {
        mType = type;
    }

    public final String getType() {
        return mType;
    }

    @Override
    public final String getAbstractSyntax() {
        return "ListTypeDefinition(" + mType + ", " + mName + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "ListTypeDefinition(" + mType + ", " + mName + ")";//mType + " " + mName;
    }

    @Override
    public final String getFormattedSyntax() {
        return "ListTypeDefinition(" + mType + ", " + mName + ")";//mType + " " + mName;
    }

    @Override
    public ListTypeDefinition getCopy() {
        return new ListTypeDefinition(mName, mType);
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {
        out.println("<ListTypeDefinition name=\"" + mName + "\" type=\"" + mType + "\"/>");
    }

    @Override
    public final void parseXML(Element element) {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
    }

    /*
    public final String getPrettyType() {
        String pretty = mType;

        // for every leading '[' append '[]' to the pretty string
        while (pretty.startsWith("[")) {
            pretty = pretty.substring(1);
            pretty += "[]";
        }

        // remove the leading 'L' and first appearance of ';'
        if ((pretty.startsWith("L")) && (!pretty.equals("Locale"))) {
            pretty = pretty.substring(1);

            int semicolonIndex = pretty.indexOf(';');

            pretty = pretty.substring(0, semicolonIndex) + pretty.substring(semicolonIndex + 1);
        }

        // remove the 'java.lang' package if present
        if (pretty.startsWith("java.lang.")) {
            pretty = pretty.substring("java.lang.".length());
        }

        return pretty;
    }
    */
}
