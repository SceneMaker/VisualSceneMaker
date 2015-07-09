package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class UnaryExp extends Expression {
    private Expression mExp;
    private Operator   mOperator;

    public enum Operator {
        Random, RemoveFirst, RemoveLast, First, Last, Clear, Size, Neg
    }

    public UnaryExp() {
        mExp      = null;
        mOperator = null;
    }

    public UnaryExp(Expression exp, Operator operator) {
        mExp      = exp;
        mOperator = operator;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

    public void setOperator(Operator value) {
        mOperator = value;
    }

    public Operator getOperator() {
        return mOperator;
    }

    public ExpType getExpType() {
        return ExpType.UN;
    }

    public String getAbstractSyntax() {
        return "UnaryExp( " + ((mOperator != null)
                               ? mOperator.name()
                               : "") + " , " + ((mExp != null)
                ? mExp.getAbstractSyntax()
                : "") + " )";
    }

    public String getConcreteSyntax() {
        String opString = "";
        String exp      = (mExp != null)
                          ? mExp.getConcreteSyntax()
                          : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
        case Random :
            opString = "Random( " + exp + " )";

            break;

        case RemoveFirst :
            opString = "RemoveFirst( " + exp + " )";

            break;

        case RemoveLast :
            opString = "RemoveLast( " + exp + " )";

            break;

        case First :
            opString = "First( " + exp + " )";

            break;

        case Last :
            opString = "Last( " + exp + " )";

            break;

        case Clear :
            opString = "Clear( " + exp + " )";

            break;

        case Size :
            opString = "Size( " + exp + " )";

            break;

        case Neg :
            opString = "- " + exp;

            break;
        }

        return opString;
    }

    public String getFormattedSyntax() {
        String opString = "";
        String exp      = (mExp != null)
                          ? mExp.getFormattedSyntax()
                          : "";

        if (mOperator == null) {
            return "";
        }

        switch (mOperator) {
        case Random :
            opString = "#p#Random( " + exp + " )";

            break;

        case RemoveFirst :
            opString = "#p#RemoveFirst( " + exp + " )";

            break;

        case RemoveLast :
            opString = "#p#RemoveLast( " + exp + " )";

            break;

        case First :
            opString = "#p#First( " + exp + " )";

            break;

        case Last :
            opString = "#p#Last( " + exp + " )";

            break;

        case Clear :
            opString = "#p#Clear( " + exp + " )";

            break;

        case Size :
            opString = "#p#Size( " + exp + " )";

            break;

        case Neg :
            opString = "#p#- " + exp;

            break;
        }

        return opString;
    }

    public UnaryExp getCopy() {
        return new UnaryExp(mExp.getCopy(), mOperator);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<" + mOperator.name() + ">").push();
        mExp.writeXML(out);
        out.pop().println("</" + mOperator.name() + ">");
    }

    public void parseXML(Element element) throws XMLParseError {
        mOperator = Operator.valueOf(element.getTagName());
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
