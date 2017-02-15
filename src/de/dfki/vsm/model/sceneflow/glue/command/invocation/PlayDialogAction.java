package de.dfki.vsm.model.sceneflow.glue.command.invocation;

import de.dfki.vsm.model.sceneflow.glue.command.Invocation;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class PlayDialogAction extends Invocation {

    // The Name Of The Dialogue Act
    private Expression mDialogueAct;

    // The List Of Additional Features
    private ArrayList<Expression> mFeatureList;

    // Construct A Playback Command
    public PlayDialogAction() {
        mDialogueAct = null;
        mFeatureList = new ArrayList<>();
    }

    // Construct A Playback Command
    public PlayDialogAction(final Expression dialogueAct) {
        mDialogueAct = dialogueAct;
        mFeatureList = new ArrayList<>();
    }

    // Construct A Playback Command
    public PlayDialogAction(final Expression dialogueAct, final ArrayList<Expression> featureList) {
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
    public final ArrayList<Expression> getArgList() {
        return mFeatureList;
    }

    // Copy The feature List
    public final ArrayList<Expression> getCopyOfArgList() {
        final ArrayList<Expression> copy = new ArrayList<>();

        for (final Expression exp : mFeatureList) {
            copy.add(exp.getCopy());
        }

        return copy;
    }

    // Get The Abstract Syntax
    @Override
    public final String getAbstractSyntax() {
        String desc = "PlayDialogAction(";

        desc += ((mDialogueAct != null)
                ? mDialogueAct.getAbstractSyntax()
                : "");

        for (int i = 0; i < mFeatureList.size(); i++) {
            desc += ", " + mFeatureList.get(i).getAbstractSyntax();
        }

        return desc + ")";
    }

    // Get The Concrete Syntax
    @Override
    public final String getConcreteSyntax() {
        String desc = "PlayDialogueAct(";

        desc += ((mDialogueAct != null)
                ? mDialogueAct.getConcreteSyntax()
                : "");

        for (int i = 0; i < mFeatureList.size(); i++) {
            desc += ", " + mFeatureList.get(i).getConcreteSyntax();
        }

        return desc + ")";
    }

    // Get The Formatted Syntax
    @Override
    public final String getFormattedSyntax() {
        String desc = "#p#PlayDialogAction ( ";

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
    public final PlayDialogAction getCopy() {
        return new PlayDialogAction(mDialogueAct.getCopy(), getCopyOfArgList());
    }

    // Write This Instance XML
    @Override
    public final void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<PlayDialogAction>").push();
        if (mDialogueAct != null) {
            mDialogueAct.writeXML(out);
        }
        for (int i = 0; i < mFeatureList.size(); i++) {
            mFeatureList.get(i).writeXML(out);
        }
        out.pop().println("</PlayDialogAction>");
    }

    // Parse This Instance XML
    @Override
    public final void parseXML(Element element) throws XMLParseError {
        final ArrayList<Expression> expList = new ArrayList<>();

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
