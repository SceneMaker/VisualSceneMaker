package de.dfki.vsm.model.sceneflow.glue.command.definition;

import de.dfki.vsm.model.sceneflow.glue.SyntaxObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ArgumentDefinition extends SyntaxObject {

    private String mName;
    private String mType;

    public ArgumentDefinition() {
        mName = null;
        mType = null;
    }

    public ArgumentDefinition(final String name, final String type) {
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

    @Override
    public final String getAbstractSyntax() {
        return "ArgumentDefinition(" + mType + " " + mName + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "ArgumentDefinition(" + mType + " " + mName + ")";//getAbstractSyntax();
    }

    @Override
    public final String getFormattedSyntax() {
        return "ArgumentDefinition(" + mType + " " + mName + ")";//
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<ArgumentDefinition name=\"" + mName + "\" type=\"" + mType + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
    }

    @Override
    public final ArgumentDefinition getCopy() {
        return new ArgumentDefinition(mName, mType);
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
     
}
