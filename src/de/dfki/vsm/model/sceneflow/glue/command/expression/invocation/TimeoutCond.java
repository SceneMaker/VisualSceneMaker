package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class TimeoutCond extends Expression {

    // private long mTimeout;
    private Expression mTimeoutExp;

    public TimeoutCond() {
    }

    public TimeoutCond(final Expression exp) {
        mTimeoutExp = exp;
    }

    public final Expression getTimeout() {
        return mTimeoutExp;
    }

    @Override
    public final String getAbstractSyntax() {
        return "TimeoutCond(" + mTimeoutExp.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "Timeout(" + mTimeoutExp.getConcreteSyntax() + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#Timeout ( " + "#c#" + mTimeoutExp.getConcreteSyntax() + " ) ";
    }

    @Override
    public final TimeoutCond getCopy() {
        return new TimeoutCond(mTimeoutExp.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<TimeoutCond>");
        mTimeoutExp.writeXML(out);
        out.println("</TimeoutCond>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                mTimeoutExp = Expression.parse(element);
            }
        });
    }
}
