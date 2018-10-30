package de.dfki.vsm.model.sceneflow.glue.command.expression.variable;

import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class MemberVariable extends VariableExpression {

    private String mName;
    private String mMember;

    public MemberVariable() {
        mName = null;
        mMember = null;
    }

    public MemberVariable(final String name, final String member) {
        mName = name;
        mMember = member;
    }

    public final String getName() {
        return mName;
    }

    public final String getMember() {
        return mMember;
    }

    @Override
    public final String getAbstractSyntax() {
        return "MemberVariable(" + mName + "," + mMember + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mName + "." + mMember;
    }

    @Override
    public final String getFormattedSyntax() {
        return mName + "." + mMember;
    }

    @Override
    public final MemberVariable getCopy() {
        return new MemberVariable(mName, mMember);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<MemberVariable name=\"" + mName + "\" member=\"" + mMember + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mMember = element.getAttribute("member");
    }
}