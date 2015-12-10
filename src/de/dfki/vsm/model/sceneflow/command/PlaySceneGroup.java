package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------



/**
 * @author Not me
 */
public class PlaySceneGroup extends AbstractCommand {
    private AbstractExpression         mArg;
    private ArrayList<AbstractExpression> mArgList;

    public PlaySceneGroup() {
        mArg     = null;
        mArgList = new ArrayList<AbstractExpression>();
    }

    public PlaySceneGroup(AbstractExpression arg) {
        mArg     = arg;
        mArgList = new ArrayList<AbstractExpression>();
    }

    public PlaySceneGroup(AbstractExpression arg, ArrayList<AbstractExpression> argList) {
        mArg     = arg;
        mArgList = argList;
    }

    public AbstractExpression getArg() {
        return mArg;
    }

    public void setArg(AbstractExpression arg) {
        mArg = arg;
    }

    public ArrayList<AbstractExpression> getArgList() {
        return mArgList;
    }

    public void setArgList(ArrayList<AbstractExpression> value) {
        mArgList = value;
    }

    public AbstractExpression getArgAt(int index) {
        return mArgList.get(index);
    }

    public void setArgAt(int index, AbstractExpression value) {
        mArgList.set(index, value);
    }

    public boolean addArg(AbstractExpression value) {
        return mArgList.add(value);
    }

    public int getSizeOfArgList() {
        return mArgList.size();
    }

    public ArrayList<AbstractExpression> getCopyOfArgList() {
        ArrayList<AbstractExpression> copy = new ArrayList<AbstractExpression>();

        for (AbstractExpression exp : mArgList) {
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
        final ArrayList<AbstractExpression> expList = new ArrayList<AbstractExpression>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            public void run(Element element) throws XMLParseError {
                expList.add(AbstractExpression.parse(element));
            }
        });
        mArg     = expList.remove(0);
        mArgList = expList;
    }
}
