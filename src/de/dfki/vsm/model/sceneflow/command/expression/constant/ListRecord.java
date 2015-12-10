package de.dfki.vsm.model.sceneflow.command.expression.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import java.util.Vector;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------



/**
 *
 * @author Not me
 */
public class ListRecord extends Constant {

    // private java.lang.String mType;
    private ArrayList<AbstractExpression> mExpList;

    public ListRecord() {

        // mType = new java.lang.String();
        mExpList = new ArrayList<AbstractExpression>();
    }

    public ListRecord( /* java.lang.String type, */ArrayList<AbstractExpression> expList) {

        // mType = type;
        mExpList = expList;
    }

    // public java.lang.String getType() {
    // return mType;
    // }
    // public void setType(java.lang.String type) {
    // mType = type;
    // }
    public ArrayList<AbstractExpression> getExpList() {
        return mExpList;
    }

    public void setExpList(ArrayList<AbstractExpression> expList) {
        mExpList = expList;
    }

    public ArrayList<AbstractExpression> getCopyOfExpList() {
        ArrayList<AbstractExpression> copy = new ArrayList<AbstractExpression>();

        for (AbstractExpression exp : mExpList) {
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

    public ListRecord getCopy() {
        return new ListRecord( /* mType, */getCopyOfExpList());
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
                AbstractExpression exp = AbstractExpression.parse(element);

                mExpList.add(exp);
            }
        });
    }
}
