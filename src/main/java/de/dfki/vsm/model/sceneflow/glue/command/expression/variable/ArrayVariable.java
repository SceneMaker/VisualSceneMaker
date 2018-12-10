package de.dfki.vsm.model.sceneflow.glue.command.expression.variable;

import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ArrayVariable extends VariableExpression {

    private String mName;
    private Expression mIndex;

    public ArrayVariable() {
        mName = null;
        mIndex = null;
    }

    public ArrayVariable(final String name, final Expression index) {
        mName = name;
        mIndex = index;
    }

    public final String getName() {
        return mName;
    }

    public final Expression getExpression() {
        return mIndex;
    }

    @Override
    public final String getAbstractSyntax() {
        return "ArrayVariable(" + mName + ", " + mIndex.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mName + "[" + mIndex.getConcreteSyntax() + "]";
    }

    @Override
    public final String getFormattedSyntax() {
        return mName + " [ " + mIndex.getFormattedSyntax() + " ] ";
    }

    @Override
    public final VariableExpression getCopy() {
        return new ArrayVariable(mName, mIndex.getCopy());
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                mIndex = Expression.parse(element);
            }
        });
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<ArrayVariable name=\"" + mName + "\">").push();
        mIndex.writeXML(out);
        out.pop().println("</ArrayVariable>");
    }
}