package de.dfki.vsm.model.sceneflow.diagram.edges;

import de.dfki.vsm.model.sceneflow.language.command.Command;
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
public final class EpsilonEdge extends AbstractEdge {

    public EpsilonEdge() {
    }

    public EpsilonEdge(
            final String target,
            final String source,
            final BasicNode targetNode,
            final BasicNode sourceNode,
            final EdgeGraphics graphics,
            final ArrayList<Command> cmdList) {
        super(target, source, targetNode, sourceNode, graphics, cmdList);
    }

    @Override
    public final Type getEdgeType() {
        return Type.EEdge;
    }

    @Override
    public final EpsilonEdge getCopy() {
        return new EpsilonEdge(
                mTarget,
                mSource,
                mTargetNode,
                mSourceNode,
                mGraphics.getCopy(),
                getCopyOfCmdList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {
        out.println("<EpsilonEdge target=\"" + mTarget + "\">");

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

        out.println("</EpsilonEdge>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mTarget = element.getAttribute("target");

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
                            mCommandList.add(Command.parse(element));
                        }
                    });
                } else {
                    throw new XMLParseError(null, null);
                }
            }
        });
    }
}
