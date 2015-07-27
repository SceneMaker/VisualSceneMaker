package de.dfki.vsm.model.sceneflow.definition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.Syntax;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class VarDef extends Syntax {
    private String     mType;
    private String     mName;
    private Expression mExp;

    public VarDef() {
        mName = new String();
        mType = new String();
        mExp  = null;
    }

    public VarDef(String name, String type, Expression exp) {
        mName = name;
        mType = type;
        mExp  = exp;
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

    public void setExp(Expression value) {
        mExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

    public String getAbstractSyntax() {
        return "VarDef(" + mType + "," + mName + "," + ((mExp != null)
                ? mExp.getAbstractSyntax()
                : "") + ")";
    }

    public String getConcreteSyntax() {
        return mType + " " + mName + " = " + ((mExp != null)
                ? mExp.getConcreteSyntax()
                : "");
    }

    public String getFormattedSyntax() {
        return "#r#" + mType + " " + "" + mName + " = " + ((mExp != null)
                ? mExp.getFormattedSyntax()
                : "");
    }

    public VarDef getCopy() {
        return new VarDef(mName, mType, ((mExp != null)
                                         ? mExp.getCopy()
                                         : null));
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Variable type=\"" + mType + "\" name =\"" + mName + "\">").push();

        if (mExp != null) {
            mExp.writeXML(out);
        }

        out.pop().println("</Variable>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mType = element.getAttribute("type");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
