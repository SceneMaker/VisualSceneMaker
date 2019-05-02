package de.dfki.vsm.model.sceneflow.glue.command.expression.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.LinkedList;

import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class ContainsList extends Expression {

    private Expression mListExp;
    private Expression mItemExp;

    public ContainsList() {
        System.err.println("Creating ContainsList");
    }

    public ContainsList(final Expression lexp, final Expression iexp) {
        mListExp = lexp;
        mItemExp = iexp;
    }

    public final Expression getListExp() {
        return mListExp;
    }

    public final Expression getItemExp() {
        return mItemExp;
    }

    @Override
    public final String getAbstractSyntax() {
        return "ContainsList(" + mListExp.getAbstractSyntax() + ", " + mItemExp.getAbstractSyntax() + ")";
    }

    @Override
    public final String getConcreteSyntax() {
        return "Contains(" + mListExp.getConcreteSyntax() + ", " + mItemExp.getConcreteSyntax() + ")";
    }

    @Override
    public final String getFormattedSyntax() {
        return "#p#Contains ( " + "#c#" + mListExp.getConcreteSyntax() + ", " + "#c#" + mItemExp.getConcreteSyntax() + " ) ";
    }

    @Override
    public final ContainsList getCopy() {
        return new ContainsList(mListExp.getCopy(), mItemExp.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<ContainsList>").push();
        //out.println("<ListExp>").push();
        mListExp.writeXML(out);
        //out.pop().println("</ListExp>");
        //out.println("<ItemExp>").push();
        mItemExp.writeXML(out);
        //out.pop().println("</ItemExp>");
        out.pop().println("</ContainsList>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        final LinkedList<Expression> expList = new LinkedList();
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public final void run(final Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mListExp = expList.getFirst();
        mItemExp = expList.getLast();
        
//        XMLParseAction.processChildNodes(element, new XMLParseAction() {
//            @Override
//            public final void run(final Element element) throws XMLParseError {
//                if (element.getTagName().equalsIgnoreCase("ListExp")) {
//                    mListExp = Expression.parse(element);
//                    System.err.println(mListExp);
//                } else if (element.getTagName().equalsIgnoreCase("ItemExp")) {
//                    mItemExp = Expression.parse(element);
//                    System.err.println(mItemExp);
//                }
//            }
//        });
    }
}
