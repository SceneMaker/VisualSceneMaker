package de.dfki.vsm.model.sceneflow.command.expression.condition.constant;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Assignment;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * A struct constant.
 *
 * @author Not me
 */
public class Struct extends Constant {

    // private java.lang.String mType;
    // private Vector<Expression> mExpList;
    private Vector<Assignment> mExpList;

    public Struct() {

        // mType = new java.lang.String();
        // mExpList = new Vector<Expression>();
        mExpList = new Vector<Assignment>();
    }

    public Struct( /* java.lang.String type,Vector<Expression> */Vector<Assignment> expList) {

        // mType = type;
        mExpList = expList;
    }

    public /* Vector<Expression> */ Vector<Assignment> getExpList() {
        return mExpList;
    }

    public Vector< /* Expression */Assignment> getCopyOfExpList() {
        Vector< /* Expression */Assignment> copy = new Vector< /* Expression */Assignment>();

        for ( /* Expression */Assignment exp : mExpList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    public ConstType getConstType() {
        return ConstType.STRUCT;
    }

    public java.lang.String getAbstractSyntax() {
        java.lang.String desc = "";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getConcreteSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return "Struct(" + desc + ")";
    }

    public java.lang.String getConcreteSyntax() {
        java.lang.String desc = "{ ";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getConcreteSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return desc + " }";
    }

    public java.lang.String getFormattedSyntax() {
        java.lang.String desc = "{ ";

        for (int i = 0; i < mExpList.size(); i++) {
            desc += mExpList.get(i).getFormattedSyntax();

            if (i != mExpList.size() - 1) {
                desc += " , ";
            }
        }

        return desc + " }";
    }

    public Struct getCopy() {
        return new Struct( /* mType, */getCopyOfExpList());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {

        // out.println("<Struct type=\"" + mType + "\">").push();
        out.println("<Struct>").push();

        for (int i = 0; i < mExpList.size(); i++) {
            mExpList.get(i).writeXML(out);
        }

        out.pop().println("</Struct>");
    }

    public void parseXML(Element element) throws XMLParseError {

        // mType = element.getAttribute("type");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {

                /* Expression */
                Assignment exp = new Assignment();    /* Expression.parse(element); */

                exp.parseXML(element);
                mExpList.add(exp);
            }
        });
    }
}
