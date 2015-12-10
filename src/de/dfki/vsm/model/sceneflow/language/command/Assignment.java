package de.dfki.vsm.model.sceneflow.language.command;

import de.dfki.vsm.model.sceneflow.language.command.expression.VariableExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class Assignment extends Command {

    // The left hand variable expression
    private VariableExpression mVariable;
    // The right hand initializer expression
    private Expression mInitializer;

    public Assignment() {
        mVariable = null;
        mInitializer = null;
    }

    public Assignment(
            final VariableExpression var,
            final Expression exp) {
        mVariable = var;
        mInitializer = exp;
    }

    public final VariableExpression getVariable() {
        return mVariable;
    }

    public final void setVariable(final VariableExpression value) {
        mVariable = value;
    }

    public final Expression getInitializer() {
        return mInitializer;
    }

    public final void setInitializer(final Expression value) {
        mInitializer = value;
    }

    @Override
    public final String getAbstractSyntax() {
        return "Assignment(" + mVariable.getAbstractSyntax()
                + "," + mInitializer.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mVariable.getConcreteSyntax()
                + " = " + mInitializer.getConcreteSyntax();
    }

    @Override
    public final String getFormattedSyntax() {
        return mVariable.getConcreteSyntax()
                + " = " + mInitializer.getConcreteSyntax();
    }

    @Override
    public final Assignment getCopy() {
        return new Assignment(
                mVariable.getCopy(),
                mInitializer.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<Assignment>").push();
        out.println("<Variable>").push();
        mVariable.writeXML(out);
        out.pop().println("</Variable>");
        out.println("<Initializer>").push();
        mInitializer.writeXML(out);
        out.pop().println("</Initializer>");
        out.pop().println("</Assignment>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                if (element.getTagName().equals("Variable")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            mVariable = VariableExpression.parse(element);
                        }
                    });
                } else if (element.getTagName().equals("Initializer")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            mInitializer = Expression.parse(element);
                        }
                    });
                } else {
                    // Error
                    throw new XMLParseError(null, null);
                }
            }
        });
    }
}
