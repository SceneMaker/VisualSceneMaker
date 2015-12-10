package de.dfki.vsm.model.sceneflow.diagram.edges;

import de.dfki.vsm.model.sceneflow.command.AbstractCommand;
import de.dfki.vsm.model.sceneflow.diagram.nodes.BasicNode;
import de.dfki.vsm.model.sceneflow.diagram.graphics.edge.EdgeGraphics;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class RandomEdge extends AbstractEdge {

// The probability
    private int mProbability = Integer.MIN_VALUE;

    public RandomEdge() {
    }

    public RandomEdge(
            final String target,
            final String source,
            final BasicNode targetNode,
            final BasicNode sourceNode,
            final EdgeGraphics graphics,
            final ArrayList<AbstractCommand> cmdList,
            int probability) {
        super(target, source, targetNode, sourceNode, graphics, cmdList);
        // Initialize the probability
        mProbability = probability;
    }

    public final int getProbability() {
        return mProbability;
    }

    public final void setProbability(final int value) {
        mProbability = value;
    }

    @Override
    public Type getEdgeType() {
        return Type.PEdge;
    }

    @Override
    public RandomEdge getCopy() {
        return new RandomEdge(
                mTarget,
                mSource,
                mTargetNode,
                mSourceNode,
                mGraphics.getCopy(),
                getCopyOfCmdList(),
                mProbability);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {

        out.println("<PEdge target=\"" + mTarget + "\" probability=\"" + mProbability + "\">").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (!mCommandList.isEmpty()) {
            out.println("<Commands>").push();

            for (int i = 0; i < mCommandList.size(); i++) {
                mCommandList.get(i).writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.pop().println("</PEdge>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mTarget = element.getAttribute("target");
        mProbability = Integer.valueOf(element.getAttribute("probability"));

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                final String tag = element.getTagName();

                if (tag.equals("Graphics")) {
                    mGraphics = new EdgeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(final Element element) throws XMLParseError {
                            mCommandList.add(AbstractCommand.parse(element));
                        }
                    });
                } else {
                    throw new XMLParseError(null, null);
                }
            }
        });
    }
}
