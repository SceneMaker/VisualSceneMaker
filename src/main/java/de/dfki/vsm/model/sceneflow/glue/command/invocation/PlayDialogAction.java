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
        StringBuilder desc = new StringBuilder("PlayDialogAction(");

        desc.append((mDialogueAct != null)
                ? mDialogueAct.getAbstractSyntax()
                : "");

        for (Expression expression : mFeatureList) {
            desc.append(", ").append(expression.getAbstractSyntax());
        }

        return desc + ")";
    }

    // Get The Concrete Syntax
    @Override
    public final String getConcreteSyntax() {
        StringBuilder desc = new StringBuilder("PlayDialogueAct(");

        desc.append((mDialogueAct != null)
                ? mDialogueAct.getConcreteSyntax()
                : "");

        for (Expression expression : mFeatureList) {
            desc.append(", ").append(expression.getConcreteSyntax());
        }

        return desc + ")";
    }

    // Get The Formatted Syntax
    @Override
    public final String getFormattedSyntax() {
        StringBuilder desc = new StringBuilder("#p#PlayDialogAction ( ");

        desc.append((mDialogueAct != null)
                ? mDialogueAct.getFormattedSyntax()
                : "");

        for (Expression expression : mFeatureList) {
            desc.append(" , ").append(expression.getFormattedSyntax());
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
        for (Expression expression : mFeatureList) {
            expression.writeXML(out);
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
