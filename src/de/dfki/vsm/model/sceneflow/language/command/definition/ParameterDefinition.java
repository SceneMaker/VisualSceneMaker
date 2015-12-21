package de.dfki.vsm.model.sceneflow.language.command.definition;

import de.dfki.vsm.model.sceneflow.language.SyntaxObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ParameterDefinition implements SyntaxObject {

    private String mName;
    private String mType;

    public ParameterDefinition() {
        mName = new String();
        mType = new String();
    }

    public ParameterDefinition(final String name, final String type) {
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
        return "ArgumentDefinition(" + mType + ", " + mName + ")";
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
    public final ParameterDefinition getCopy() {
        return new ParameterDefinition(mName, mType);
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {
        out.println("<ArgumentDefinition name=\"" + mName + "\" type=\"" + mType + "\"/>");
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
