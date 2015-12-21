package de.dfki.vsm.model.sceneflow.language.command.definition.datatype;

import de.dfki.vsm.model.sceneflow.language.command.definition.DatatypeDefinition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class ListTypeDefinition extends DatatypeDefinition {

    private String mName;
    private String mType;

    public ListTypeDefinition() {
        mName = new String();
        mType = new String();
    }

    public ListTypeDefinition(final String name, final String type) {
        mName = name;
        mType = type;
    }

    public final void setName(final String value) {
        mName = value;
    }

    public final String getName() {
        return mName;
    }

    public final void setType(final String value) {
        mType = value;
    }

    public final String getType() {
        return mType;
    }

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

    @Override
    public final String getAbstractSyntax() {
        return "ListTypeDefinition(" + mType + ", " + mName + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mType + " " + mName;
    }

    @Override
    public final String getFormattedSyntax() {
        return getConcreteSyntax();
    }

    @Override
    public final ListTypeDefinition getCopy() {
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

    @Override
    public String toString() {
        return getAbstractSyntax();
    }

}
