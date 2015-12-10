package de.dfki.vsm.model.sceneflow.diagram.edges;

import de.dfki.vsm.model.sceneflow.command.AbstractCommand;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
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
public final class TimeoutEdge extends AbstractEdge {

    private long mTimeout = Long.MIN_VALUE;
    private AbstractExpression mExpression = null;

    public TimeoutEdge() {
    }

    public TimeoutEdge(
            final String target,
            final String source,
            final BasicNode targetNode,
            final BasicNode sourceNode,
            final EdgeGraphics graphics,
            final ArrayList<AbstractCommand> cmdList,
            long timeout) {
        super(target, source, targetNode, sourceNode, graphics, cmdList);
        // Initialize the timeout value
        mTimeout = timeout;
    }

    public final long getTimeout() {
        return mTimeout;
    }

    public final void setTimeout(final long value) {
        mTimeout = value;
    }

    @Override
    public final Type getEdgeType() {
        return Type.TEdge;
    }

    @Override
    public TimeoutEdge getCopy() {
        return new TimeoutEdge(
                mTarget,
                mSource,
                mTargetNode,
                mSourceNode,
                mGraphics.getCopy(),
                getCopyOfCmdList(),
                mTimeout);
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) throws XMLWriteError {

        out.println("<TEdge target=\"" + mTarget + "\" timeout=\"" + mTimeout + "\">");

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

        out.println("</TEdge>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        mTarget = element.getAttribute("target");
        mTimeout = Long.valueOf(element.getAttribute("timeout"));

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
