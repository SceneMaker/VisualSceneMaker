package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Vector;

/**
 * @author Not me
 */
public class PlaySceneGroup extends Command {
    private Expression         mArg;
    private Vector<Expression> mArgList;

    public PlaySceneGroup() {
        mArg     = null;
        mArgList = new Vector<Expression>();
    }

    public PlaySceneGroup(Expression arg) {
        mArg     = arg;
        mArgList = new Vector<Expression>();
    }

    public PlaySceneGroup(Expression arg, Vector<Expression> argList) {
        mArg     = arg;
        mArgList = argList;
    }

    public Expression getArg() {
        return mArg;
    }

    public void setArg(Expression arg) {
        mArg = arg;
    }

    public Vector<Expression> getArgList() {
        return mArgList;
    }

    public void setArgList(Vector<Expression> value) {
        mArgList = value;
    }

    public Expression getArgAt(int index) {
        return mArgList.get(index);
    }

    public void setArgAt(int index, Expression value) {
        mArgList.set(index, value);
    }

    public boolean addArg(Expression value) {
        return mArgList.add(value);
    }

    public int getSizeOfArgList() {
        return mArgList.size();
    }

    public Vector<Expression> getCopyOfArgList() {
        Vector<Expression> copy = new Vector<Expression>();

        for (Expression exp : mArgList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    public CmdType getCmdType() {
        return CmdType.PSG;
    }

    public String getAbstractSyntax() {
        String desc = "PlaySceneGroup" + " ( ";

        desc += ((mArg != null)
                 ? mArg.getAbstractSyntax()
                 : "");

        for (int i = 0; i < mArgList.size(); i++) {
            desc += " , " + mArgList.get(i).getAbstractSyntax();
        }

        return desc + " ) ";
    }

    public String getConcreteSyntax() {
        String desc = "PlaySceneGroup" + " ( ";

        desc += ((mArg != null)
                 ? mArg.getConcreteSyntax()
                 : "");

        for (int i = 0; i < mArgList.size(); i++) {
            desc += " , " + mArgList.get(i).getConcreteSyntax();
        }

        return desc + " ) ";
    }

    public String getFormattedSyntax() {
        String desc = "#p#PlaySceneGroup" + " ( ";

        desc += ((mArg != null)
                 ? mArg.getFormattedSyntax()
                 : "");

        for (int i = 0; i < mArgList.size(); i++) {
            desc += " , " + mArgList.get(i).getFormattedSyntax();
        }

        return desc + " ) ";
    }

    public PlaySceneGroup getCopy() {
        return new PlaySceneGroup(mArg.getCopy(), getCopyOfArgList());
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlaySceneGroup>").push();

        if (mArg != null) {
            mArg.writeXML(out);
        }

        for (int i = 0; i < mArgList.size(); i++) {
            mArgList.get(i).writeXML(out);
        }

        out.pop().println("</PlaySceneGroup>");
    }

    public void parseXML(Element element) throws XMLParseError {
        final Vector<Expression> expList = new Vector<Expression>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mArg     = expList.remove(0);
        mArgList = expList;
    }
}
