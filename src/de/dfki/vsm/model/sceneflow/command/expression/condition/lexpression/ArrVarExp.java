
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.model.sceneflow.command.expression.condition.lexpression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

/**
 *
 * @author Not me
 */
public class ArrVarExp extends LExpression {
    private String     mName;
    private Expression mIndex;

    public ArrVarExp() {
        mName  = new String();
        mIndex = null;
    }

    public ArrVarExp(String name, Expression index) {
        mName  = name;
        mIndex = index;
    }

    public String getName() {
        return mName;
    }

    public Expression getExp() {
        return mIndex;
    }

    @Override
    public String getAbstractSyntax() {
        return mName + " [ " + mIndex.getAbstractSyntax() + " ] ";
    }

    @Override
    public String getConcreteSyntax() {
        return mName + " [ " + mIndex.getConcreteSyntax() + " ] ";
    }

    public String getFormattedSyntax() {
        return mName + " [ " + mIndex.getFormattedSyntax() + " ] ";
    }

    @Override
    public LExpType getLExpType() {
        return LExpType.ARRAY;
    }

    @Override
    public LExpression getCopy() {
        return new ArrVarExp(mName, mIndex.getCopy());
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                mIndex = Expression.parse(element);
            }
        });
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Field name=\"" + mName + "\">").push();
        mIndex.writeXML(out);
        out.pop().println("</Field>");
    }
}
