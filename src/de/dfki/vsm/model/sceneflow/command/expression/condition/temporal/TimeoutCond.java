package de.dfki.vsm.model.sceneflow.command.expression.condition.temporal;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 * A timeout condition.
 *
 * @author Not me
 */
public class TimeoutCond extends TemporalCond {

    // private long mTimeout;
    private Expression mTimeoutExp;

    public TimeoutCond() {}

    public TimeoutCond(Expression exp) {
        mTimeoutExp = exp;
    }

    public Expression getTimeout() {
        return mTimeoutExp;
    }

    public TemporalType getTemporalType() {
        return TemporalType.TIMEOUT;
    }

    public String getAbstractSyntax() {
        return "TimeoutCond(" + mTimeoutExp.getAbstractSyntax() + ")";
    }

    public String getConcreteSyntax() {
        return "Timeout(" + mTimeoutExp.getConcreteSyntax() + ")";
    }

    public String getFormattedSyntax() {
        return "";
    }

    public TimeoutCond getCopy() {
        return new TimeoutCond(mTimeoutExp.getCopy());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<TimeoutCondition>");
        mTimeoutExp.writeXML(out);
        out.println("</TimeoutCondition>");
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mTimeoutExp = Expression.parse(element);
            }
        });
    }
}
