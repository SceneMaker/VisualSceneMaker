package de.dfki.vsm.model.sceneflow.command.expression.condition;

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
 * An empty condition
 *
 * @author Not me
 */
public class ContainsCond extends Condition {
    private Expression mLeftExp;
    private Expression mRightExp;

    public ContainsCond() {
        mLeftExp  = null;
        mRightExp = null;
    }

    public ContainsCond(Expression lexp, Expression rexp) {
        mLeftExp  = lexp;
        mRightExp = rexp;
    }

    public Expression getListExp() {
        return mLeftExp;
    }

    public Expression getElementExp() {
        return mRightExp;
    }

    public void setListExp(Expression value) {
        mLeftExp = value;
    }

    public void setElementExp(Expression value) {
        mRightExp = value;
    }

    public CondType getCondType() {
        return CondType.CONTAINS;
    }

    public String getAbstractSyntax() {
        return "Contains ( " + ((mLeftExp != null)
                                ? mLeftExp.getAbstractSyntax()
                                : "") + " , " + ((mRightExp != null)
                ? mRightExp.getAbstractSyntax()
                : "") + " ) ";
    }

    public String getConcreteSyntax() {
        return "Contains ( " + ((mLeftExp != null)
                                ? mLeftExp.getConcreteSyntax()
                                : "") + " , " + ((mRightExp != null)
                ? mRightExp.getConcreteSyntax()
                : "") + " ) ";
    }

    public String getFormattedSyntax() {
        return "Contains ( " + ((mLeftExp != null)
                                ? mLeftExp.getFormattedSyntax()
                                : "") + " , " + ((mRightExp != null)
                ? mRightExp.getFormattedSyntax()
                : "") + " ) ";
    }

    public ContainsCond getCopy() {
        return new ContainsCond(mLeftExp.getCopy(), mRightExp.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Contains>").push();
        mLeftExp.writeXML(out);
        mRightExp.writeXML(out);
        out.pop().println("</Contains>");
    }

    public void parseXML(Element element) throws XMLParseError {
        final Vector<Expression> expList = new Vector<Expression>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mLeftExp  = expList.firstElement();
        mRightExp = expList.lastElement();
    }
}
