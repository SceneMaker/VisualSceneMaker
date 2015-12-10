package de.dfki.vsm.model.sceneflow.definition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * A paramater definition.
 *
 * @author Not me
 */
public class ParamDef extends Syntax {
    private String mName;
    private String mType;

    public ParamDef() {
        mName = new String();
        mType = new String();
    }

    public ParamDef(String name, String type) {
        mName = name;
        mType = type;
    }

    public void setName(String value) {
        mName = value;
    }

    public String getName() {
        return mName;
    }

    public void setType(String value) {
        mType = value;
    }

    public String getType() {
        return mType;
    }

    public String getPrettyType() {
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

    public String getAbstractSyntax() {
        return "ParamDef(" + mType + " " + mName + ")";
    }

    public String getConcreteSyntax() {
        return getAbstractSyntax();
    }

    public String getFormattedSyntax() {
        return "";
    }

    public ParamDef getCopy() {
        return new ParamDef(mName, mType);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Argument name=\"" + mName + "\" type=\"" + mType + "\"/>");
    }

    public void parseXML(Element element) {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
    }
}
