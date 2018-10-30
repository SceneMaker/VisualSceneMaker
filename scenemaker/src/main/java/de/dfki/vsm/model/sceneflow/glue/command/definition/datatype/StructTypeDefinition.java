package de.dfki.vsm.model.sceneflow.glue.command.definition.datatype;

import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class StructTypeDefinition extends DataTypeDefinition {

    private ArrayList<MemberDefinition> mMemberList
            = new ArrayList();

    public StructTypeDefinition() {
        mName = new String();
    }

    public StructTypeDefinition(final String name, final ArrayList list) {
        mName = name;
        mMemberList = list;
    }

    @Override
    public Flavour getFlavour() {
        return Flavour.Struct;
    }

    public final MemberDefinition getMemberDefAt(final int index) {
        return mMemberList.get(index);
    }

    public final void setMemberDefAt(final int index, final MemberDefinition def) {
        mMemberList.set(index, def);
    }

    public final void addMemberDef(final MemberDefinition def) {
        mMemberList.add(def);
    }

    public final void removeMemberDefAt(final int index) {
        mMemberList.remove(index);
    }

    public final void setMemberList(final ArrayList list) {
        mMemberList = list;
    }

    public final ArrayList<MemberDefinition> getMemberList() {
        return mMemberList;
    }

    public final ArrayList getCopyOfParamList() {
        final ArrayList copy = new ArrayList();
        for (MemberDefinition def : mMemberList) {
            copy.add(def.getCopy());
        }
        return copy;
    }

    @Override
    public final String getAbstractSyntax() {
        return "StructTypeDefinition(" + mName + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "StructTypeDefinition(" + mName + ")";//mName;
    }

    @Override
    public final String getFormattedSyntax() {
        return "StructTypeDefinition(" + mName + ")";//mName;
    }

    @Override
    public final StructTypeDefinition getCopy() {
        return new StructTypeDefinition(mName, getCopyOfParamList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<StructTypeDefinition name=\"" + mName + "\">").push();
        for (int i = 0; i < mMemberList.size(); i++) {
            mMemberList.get(i).writeXML(out);
        }
        out.pop().println("</StructTypeDefinition>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                MemberDefinition member = new MemberDefinition();
                member.parseXML(element);
                mMemberList.add(member);
            }
        });
    }
}