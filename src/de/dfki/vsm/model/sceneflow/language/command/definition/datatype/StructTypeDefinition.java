package de.dfki.vsm.model.sceneflow.language.command.definition.datatype;

import de.dfki.vsm.model.sceneflow.language.command.definition.DatatypeDefinition;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class StructTypeDefinition extends DatatypeDefinition {

    private String mName;
    private ArrayList<MemberDefinition> mMemberList
            = new ArrayList();

    public StructTypeDefinition() {
        mName = new String();
    }

    public StructTypeDefinition(final String name, final ArrayList memberList) {
        mName = name;
        mMemberList = memberList;
    }

    public final void setName(final String value) {
        mName = value;
    }

    public final String getName() {
        return mName;
    }

    public final ArrayList getArgList() {
        return mMemberList;
    }

    public final void setArgList(final ArrayList value) {
        mMemberList = value;
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
        return mName;
    }

    @Override
    public final String getFormattedSyntax() {
        return getConcreteSyntax();
    }

    @Override
    public final StructTypeDefinition getCopy() {
        return new StructTypeDefinition(mName, getCopyOfParamList());
    }

    @Override
    public final void writeXML(IOSIndentWriter out) {

    }

    @Override
    public final void parseXML(Element element) {

    }

    @Override
    public String toString() {
        return getAbstractSyntax();
    }

}
