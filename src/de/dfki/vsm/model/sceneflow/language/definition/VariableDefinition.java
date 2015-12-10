package de.dfki.vsm.model.sceneflow.language.definition;

import de.dfki.vsm.model.sceneflow.language.SyntaxObject;
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class VariableDefinition implements SyntaxObject {

    private String mType;
    private String mName;
    private Expression mExp;

    public VariableDefinition() {
        mName = new String();
        mType = new String();
        mExp = null;
    }

    public VariableDefinition(
            final String name,
            final String type,
            final Expression exp) {
        mName = name;
        mType = type;
        mExp = exp;
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

    public final void setExp(final Expression value) {
        mExp = value;
    }

    public final Expression getExp() {
        return mExp;
    }

    @Override
    public final String getAbstractSyntax() {
        return "VariableDefinition(" + mType + ", " + mName + ", " + mExp.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mType + " " + mName + " = " + mExp.getConcreteSyntax();
    }

    @Override
    public final String getFormattedSyntax() {
        return "#r#" + mType + " " + mName + " = " + mExp.getFormattedSyntax();
    }

    @Override
    public VariableDefinition getCopy() {
        return new VariableDefinition(mName, mType, mExp.getCopy());
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<VariableDefinition type=\"" + mType + "\" name =\"" + mName + "\">").push();
        if (mExp != null) {
            mExp.writeXML(out);
        }
        out.pop().println("</VariableDefinition>");
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
