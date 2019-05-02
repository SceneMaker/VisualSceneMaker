package de.dfki.vsm.model.sceneflow.glue.command.definition.datatype;

import de.dfki.vsm.model.sceneflow.glue.SyntaxObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class MemberDefinition extends SyntaxObject {

    private String mType;
    private String mName;

    public MemberDefinition() {
        mName = null;
        mType = null;
    }

    public MemberDefinition(final String name, final String type) {
        mName = name;
        mType = type;
    }

    public final void setName(final String name) {
        mName = name;
    }

    public final String getName() {
        return mName;
    }

    public final void setType(final String type) {
        mType = type;
    }

    public final String getType() {
        return mType;
    }

    @Override
    public final String getAbstractSyntax() {
        return "MemberDefinition(" + mName + ", " + mType + ")";
    }

    @Override
    public String getConcreteSyntax() {
        return mName + " : " + mType;
    }

    @Override
    public String getFormattedSyntax() {
        return mName + " : #r#" + mType;
    }

    @Override
    public MemberDefinition getCopy() {
        return new MemberDefinition(mName, mType);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<MemberDefinition type=\"" + mType + "\" name =\"" + mName + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
    }
}