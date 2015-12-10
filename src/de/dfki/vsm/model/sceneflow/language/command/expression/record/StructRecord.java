package de.dfki.vsm.model.sceneflow.language.command.expression.record;

import de.dfki.vsm.model.sceneflow.language.command.Assignment;
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.model.sceneflow.language.command.expression.LiteralExpression.ConstType;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;



/**
 * A struct constant.
 *
 * @author Not me
 */
public class StructRecord extends Expression {


    private ArrayList<Assignment> mExpList;

    public StructRecord() {

        // mType = new java.lang.String();
        // mExpList = new Vector<Expression>();
        mExpList = new ArrayList<Assignment>();
    }

    public StructRecord( /* java.lang.String type,Vector<Expression> */ArrayList<Assignment> expList) {

        // mType = type;
        mExpList = expList;
    }

//    @Override
//    public ExpType getExpType() {
//        return ExpType.STRUCT;
//    }
    
    public /* Vector<Expression> */ ArrayList<Assignment> getExpList() {
        return mExpList;
    }

    public ArrayList< /* Expression */Assignment> getCopyOfExpList() {
        ArrayList< /* Expression */Assignment> copy = new ArrayList< /* Expression */Assignment>();

        for ( /* Expression */Assignment exp : mExpList) {
            copy.add(exp.getCopy());
        }

        return copy;
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

    public StructRecord getCopy() {
        return new StructRecord( /* mType, */getCopyOfExpList());
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
