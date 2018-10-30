package de.dfki.vsm.model.sceneflow.glue.command.expression.variable;

import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class SimpleVariable extends VariableExpression {

    private String mName;

    public SimpleVariable() {
        mName = null;
    }

    public SimpleVariable(final String name) {
        mName = name;
    }

    public final String getName() {
        return mName;
    }

    @Override
    public final String getAbstractSyntax() {
        return "SimpleVariable(" + mName + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return mName;
    }

    @Override
    public final String getFormattedSyntax() {
        return mName;
    }

    @Override
    public final SimpleVariable getCopy() {
        return new SimpleVariable(mName);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<SimpleVariable name=\"" + mName + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) {
        mName = element.getAttribute("name");
    }
}
