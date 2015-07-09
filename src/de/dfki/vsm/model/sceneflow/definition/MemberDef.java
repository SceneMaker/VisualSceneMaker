package de.dfki.vsm.model.sceneflow.definition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * A member definition.
 *
 * @author Not me
 */
public class MemberDef extends Syntax {
    private String mType;
    private String mName;

    public MemberDef() {
        mName = new String();
        mType = new String();
    }

    public MemberDef(String name, String type) {
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

    public String getAbstractSyntax() {
        return "Member(" + mName + "," + mType + ")";
    }

    public String getConcreteSyntax() {
        return mName + " : " + mType;
    }

    public String getFormattedSyntax() {
        return "" + mName + " : #r#" + mType;
    }

    public MemberDef getCopy() {
        return new MemberDef(mName, mType);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Member type=\"" + mType + "\" name =\"" + mName + "\"/>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
    }
}
