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
public class PlayDialogueAct extends Command {

    // The Name Of The Dialogue Act
    private Expression mDialogueAct;

    // The List Of Additional Features
    private Vector<Expression> mFeatureList;

    // Construct A Playback Command
    public PlayDialogueAct() {
        mDialogueAct = null;
        mFeatureList = new Vector<>();
    }

    // Construct A Playback Command
    public PlayDialogueAct(final Expression dialogueAct) {
        mDialogueAct = dialogueAct;
        mFeatureList = new Vector<>();
    }

    // Construct A Playback Command
    public PlayDialogueAct(final Expression dialogueAct, final Vector<Expression> featureList) {
        mDialogueAct = dialogueAct;
        mFeatureList = featureList;
    }

    // Get The Dialogue Act
    public final Expression getArg() {
        return mDialogueAct;
    }

    // Set The Dialogue Act
    public final void setDialogueAct(final Expression dialogueAct) {
        mDialogueAct = dialogueAct;
    }

    // Get The Feature List
    public final Vector<Expression> getArgList() {
        return mFeatureList;
    }

    // Set The Feature List
    // public final void setFeatureList(final Vector<Expression> value) {
    // mFeatureList = value;
    // }
    // Get Specific Feature At
    // public final Expression getFeatureAt(final int index) {
    // return mFeatureList.get(index);
    // }
    // Set Specific Feature At
    // public final void setFeatureAt(final int index, final Expression value) {
    // mFeatureList.set(index, value);
    // }
    // Add New Specific Feature
    // public final boolean addFeature(final Expression value) {
    // return mFeatureList.add(value);
    // }
    // Get Number Of Features
    // public final int getFeatureCount() {
    // return mFeatureList.size();
    // }
    // Copy The feature List
    public final Vector<Expression> getCopyOfArgList() {
        final Vector<Expression> copy = new Vector<>();

        for (final Expression exp : mFeatureList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    // Get The Command Type
    @Override
    public final CmdType getCmdType() {
        return CmdType.PDA;
    }

    // Get The Abstract Syntax
    @Override
    public final String getAbstractSyntax() {
        String desc = "PlayDialogueAct" + " ( ";

        desc += ((mDialogueAct != null)
                 ? mDialogueAct.getAbstractSyntax()
                 : "");

        for (int i = 0; i < mFeatureList.size(); i++) {
            desc += " , " + mFeatureList.get(i).getAbstractSyntax();
        }

        return desc + " ) ";
    }

    // Get The Concrete Syntax
    @Override
    public final String getConcreteSyntax() {
        String desc = "PlayDialogueAct" + " ( ";

        desc += ((mDialogueAct != null)
                 ? mDialogueAct.getConcreteSyntax()
                 : "");

        for (int i = 0; i < mFeatureList.size(); i++) {
            desc += " , " + mFeatureList.get(i).getConcreteSyntax();
        }

        return desc + " ) ";
    }

    // Get The Formatted Syntax
    @Override
    public final String getFormattedSyntax() {
        String desc = "#p#PlayDialogueAct" + " ( ";

        desc += ((mDialogueAct != null)
                 ? mDialogueAct.getFormattedSyntax()
                 : "");

        for (int i = 0; i < mFeatureList.size(); i++) {
            desc += " , " + mFeatureList.get(i).getFormattedSyntax();
        }

        return desc + " ) ";
    }

    // Get Copy Of This Instance
    @Override
    public final PlayDialogueAct getCopy() {
        return new PlayDialogueAct(mDialogueAct.getCopy(), getCopyOfArgList());
    }

    // Write This Instance XML
    @Override
    public final void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlayDialogueAct>").push();

        if (mDialogueAct != null) {
            mDialogueAct.writeXML(out);
        }

        for (int i = 0; i < mFeatureList.size(); i++) {
            mFeatureList.get(i).writeXML(out);
        }

        out.pop().println("</PlayDialogueAct>");
    }

    // Parse This Instance XML
    @Override
    public final void parseXML(Element element) throws XMLParseError {
        final Vector<Expression> expList = new Vector<>();

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                expList.add(Expression.parse(element));
            }
        });
        mDialogueAct = expList.remove(0);
        mFeatureList = expList;
    }
}
