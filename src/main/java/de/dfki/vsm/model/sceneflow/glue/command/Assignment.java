package de.dfki.vsm.model.sceneflow.glue.command;

import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class Assignment extends Command {

    private VariableExpression mLeftExpression;
    private Expression mInitExpression;

    public Assignment() {
        mLeftExpression = null;
        mInitExpression = null;
    }

    public Assignment(final VariableExpression lExp, final Expression iExp) {
        mLeftExpression = lExp;
        mInitExpression = iExp;
    }

    public final VariableExpression getLeftExpression() {
        return mLeftExpression;
    }

    public final void setLeftExpression(final VariableExpression value) {
        mLeftExpression = value;
    }

    public final Expression getInitExpression() {
        return mInitExpression;
    }

    public void setInitExpression(final Expression iExp) {
        mInitExpression = iExp;
    }

    @Override
    public final String getAbstractSyntax() {
        return "Assignment(" + ((mLeftExpression != null)
                ? mLeftExpression.getAbstractSyntax()
                : "") + " = " + ((mInitExpression != null)
                        ? mInitExpression.getAbstractSyntax()
                        : "") + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return ((mLeftExpression != null)
                ? mLeftExpression.getConcreteSyntax()
                : "") + " = " + ((mInitExpression != null)
                        ? mInitExpression.getConcreteSyntax()
                        : "");
    }

    @Override
    public final String getFormattedSyntax() {
        return ((mLeftExpression != null)
                ? mLeftExpression.getFormattedSyntax()
                : "") + " = " + ((mInitExpression != null)
                        ? mInitExpression.getFormattedSyntax()
                        : "");
    }

    @Override
    public final Assignment getCopy() {
        return new Assignment(
                mLeftExpression.getCopy(),
                mInitExpression.getCopy());
    }

    @Override
    public void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<Assignment>").push();
        mLeftExpression.writeXML(out);
        out.println("<Expression>").push();
        mInitExpression.writeXML(out);
        out.pop().println("</Expression>");
        out.pop().println("</Assignment>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                if (element.getTagName().equals("Expression")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public final void run(final Element element) throws XMLParseError {
                            mInitExpression = Expression.parse(element);
                        }
                    });
                } else {
                    mLeftExpression = VariableExpression.parse(element);
                }
            }
        });
    }
}
