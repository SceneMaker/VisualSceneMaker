package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression.LExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class Assignment extends Command {
    private LExpression mLExp;
    private Expression  mExp;

    public Assignment() {
        mLExp = null;
        mExp  = null;
    }

    public Assignment(LExpression lExp, Expression exp) {
        mLExp = lExp;
        mExp  = exp;
    }

    public LExpression getLExp() {
        return mLExp;
    }

    public void setLExp(LExpression value) {
        mLExp = value;
    }

    public Expression getExp() {
        return mExp;
    }

    public void setExp(Expression value) {
        mExp = value;
    }

    public CmdType getCmdType() {
        return CmdType.ASGN;
    }

    public String getAbstractSyntax() {
        return "Assignment(" + ((mLExp != null)
                                ? mLExp.getAbstractSyntax()
                                : "") + " = " + ((mExp != null)
                ? mExp.getAbstractSyntax()
                : "") + ")";
    }

    public String getConcreteSyntax() {
        return ((mLExp != null)
                ? mLExp.getConcreteSyntax()
                : "") + " = " + ((mExp != null)
                                 ? mExp.getConcreteSyntax()
                                 : "");
    }

    public String getFormattedSyntax() {
        return ((mLExp != null)
                ? mLExp.getFormattedSyntax()
                : "") + " = " + ((mExp != null)
                                 ? mExp.getFormattedSyntax()
                                 : "");
    }

    public Assignment getCopy() {
        return new Assignment(mLExp.getCopy(), mExp.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Assign>").push();
        mLExp.writeXML(out);
        out.println("<Expression>").push();
        mExp.writeXML(out);
        out.pop().println("</Expression>");
        out.pop().println("</Assign>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                if (element.getTagName().equals("Expression")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        public void run(Element element) throws XMLParseError {
                            mExp = Expression.parse(element);
                        }
                    });
                } else {
                    mLExp = LExpression.parse(element);
                }
            }
        });
    }
}
