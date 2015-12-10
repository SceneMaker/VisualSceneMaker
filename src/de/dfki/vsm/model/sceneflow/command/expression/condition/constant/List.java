package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 *
 * @author Not me
 */
public class List extends Constant {

    // private java.lang.String mType;
    private Vector<Expression> mExpList;

    public List() {

        // mType = new java.lang.String();
        mExpList = new Vector<Expression>();
    }

    public List( /* java.lang.String type, */Vector<Expression> expList) {

        // mType = type;
        mExpList = expList;
    }

    // public java.lang.String getType() {
    // return mType;
    // }
    // public void setType(java.lang.String type) {
    // mType = type;
    // }
    public Vector<Expression> getExpList() {
        return mExpList;
    }

    public void setExpList(Vector<Expression> expList) {
        mExpList = expList;
    }

    public Vector<Expression> getCopyOfExpList() {
        Vector<Expression> copy = new Vector<Expression>();

        for (Expression exp : mExpList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    public ConstType getConstType() {
        return ConstType.LIST;
    }

    public java.lang.String getAbstractSyntax() {
        java.lang.String desc = "";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getConcreteSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return "List(" + desc + ")";
    }

    public java.lang.String getConcreteSyntax() {
        java.lang.String desc = /* mType + */ "[ ";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getConcreteSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return desc + " ]";
    }

    public java.lang.String getFormattedSyntax() {
        java.lang.String desc = /* mType + */ "[ ";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getFormattedSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return desc + " ]";
    }

    public List getCopy() {
        return new List( /* mType, */getCopyOfExpList());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        // out.println("<List type=\"" + mType + "\">").push();
        out.println("<List>").push();

        for (int i = 0; i < mExpList.size(); i++) {
            mExpList.get(i).writeXML(out);
        }

        out.pop().println("</List>");
    }

    public void parseXML(Element element) throws XMLParseError {

        // mType = element.getAttribute("type");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                Expression exp = Expression.parse(element);

                mExpList.add(exp);
            }
        });
    }
}
