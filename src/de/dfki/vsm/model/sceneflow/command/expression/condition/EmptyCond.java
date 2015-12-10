package de.dfki.vsm.model.sceneflow.command.expression.condition;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * An empty condition
 *
 * @author Not me
 */
public class EmptyCond extends Condition {
    private Expression mExp;

    public EmptyCond() {
        mExp = null;
    }

    public EmptyCond(Expression exp) {
        mExp = exp;
    }

    public Expression getExp() {
        return mExp;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public CondType getCondType() {
        return CondType.EMPTY;
    }

    public String getAbstractSyntax() {
        return "Empty ( " + ((mExp != null)
                             ? mExp.getAbstractSyntax()
                             : "") + " ) ";
    }

    public String getConcreteSyntax() {
        return "Empty ( " + ((mExp != null)
                             ? mExp.getConcreteSyntax()
                             : "") + " ) ";
    }

    public String getFormattedSyntax() {
        return "";
    }

    public EmptyCond getCopy() {
        return new EmptyCond(mExp.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Empty>").push();
        mExp.writeXML(out);
        out.pop().println("</Empty>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mExp = Expression.parse(element);
            }
        });
    }
}
