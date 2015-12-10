package de.dfki.vsm.model.sceneflow.definition.type;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * A struct type definition.
 *
 * @author Not me
 */
public class StructTypeDef extends TypeDef {
    private Vector<MemberDef> mMemberDefList;

    public StructTypeDef() {
        mMemberDefList = new Vector<MemberDef>();
    }

    public StructTypeDef(String name, Vector<MemberDef> memberList) {
        super(name);
        mMemberDefList = memberList;
    }

    public Vector<MemberDef> getMemberDefList() {
        return mMemberDefList;
    }

    public void setMemberDefList(Vector<MemberDef> value) {
        mMemberDefList = value;
    }

    public int getSizeOfMemberDefList() {
        return mMemberDefList.size();
    }

    public Vector<MemberDef> getCopyOfMemberDefList() {
        Vector<MemberDef> copy = new Vector<MemberDef>();

        for (MemberDef member : mMemberDefList) {
            copy.add(member.getCopy());
        }

        return copy;
    }

    public void addMemberDef(MemberDef value) {
        mMemberDefList.add(value);
    }

    public MemberDef removeMemberDefAt(int index) {
        return mMemberDefList.remove(index);
    }

    public MemberDef getMemberDefAt(int index) {
        return mMemberDefList.get(index);
    }

    public MemberDef setMemberDefAt(int index, MemberDef memberDef) {
        return mMemberDefList.set(index, memberDef);
    }

    public String getAbstractSyntax() {
        String desc = "[";

        for (int i = 0; i < mMemberDefList.size(); i++) {
            desc += mMemberDefList.get(i).getAbstractSyntax();

            if (i != mMemberDefList.size() - 1) {
                desc += ",";
            }
        }

        desc += "]";

        return "Struct(" + mName + "," + desc + ")";
    }

    public String getConcreteSyntax() {
        String desc = "";

        for (int i = 0; i < mMemberDefList.size(); i++) {
            desc += mMemberDefList.get(i).getConcreteSyntax();

            if (i != mMemberDefList.size() - 1) {
                desc += ", ";
            }
        }

        return "Struct " + mName + " : { " + desc + " }";
    }

    public String getFormattedSyntax() {
        String desc = "";

        for (int i = 0; i < mMemberDefList.size(); i++) {
            desc += mMemberDefList.get(i).getFormattedSyntax();

            if (i != mMemberDefList.size() - 1) {
                desc += ", ";
            }
        }

        return "#h#Struct " + "#t#" + mName + " : { " + desc + " }";
    }

    public StructTypeDef getCopy() {
        return new StructTypeDef(mName, getCopyOfMemberDefList());
    }

    public Flavour getFlavour() {
        return Flavour.Struct;
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<StructType name=\"" + mName + "\">").push();

        for (int i = 0; i < mMemberDefList.size(); i++) {
            mMemberDefList.get(i).writeXML(out);
        }

        out.pop().println("</StructType>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                MemberDef member = new MemberDef();

                member.parseXML(element);
                mMemberDefList.add(member);
            }
        });
    }
}
