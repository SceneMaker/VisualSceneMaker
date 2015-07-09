package de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * A member of a struct.
 *
 * @author Not me
 */
public class MemVarExp extends LExpression {
    private String mName;
    private String mMember;

    public MemVarExp() {
        mName   = new String();
        mMember = new String();
    }

    public MemVarExp(String name, String member) {
        mName   = name;
        mMember = member;
    }

    public String getName() {
        return mName;
    }

    public String getMemberName() {
        return mMember;
    }

    public LExpType getLExpType() {
        return LExpType.MEMBER;
    }

    public String getAbstractSyntax() {
        return "MemberExp(" + mName + "," + mMember + ")";
    }

    public String getConcreteSyntax() {
        return mName + "." + mMember;
    }

    public String getFormattedSyntax() {
        return mName + "." + mMember;
    }

    public MemVarExp getCopy() {
        return new MemVarExp(mName, mMember);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<Member name=\"" + mName + "\" member=\"" + mMember + "\">").push();
        out.pop().println("</Member>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mName   = element.getAttribute("name");
        mMember = element.getAttribute("member");

        /*
         * ParseAction.processChildNodes(element, new ParseAction() {
         *
         * public void run(Element element) throws ParseException {
         * mMember = Expression.parse(element);
         * }
         * });
         */
    }
}
