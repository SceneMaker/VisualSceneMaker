package de.dfki.vsm.model.sceneflow.chart.edge;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.glue.command.Command;
import de.dfki.vsm.model.sceneflow.chart.graphics.edge.EdgeGraphics;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.tpl.TPLTuple;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * @author Gregor Mehlmann
 */
public class RandomEdge extends AbstractEdge {
    protected int mProbability = Integer.MIN_VALUE;

    public RandomEdge() {}

    public RandomEdge(String target, String source, BasicNode targetNode, BasicNode sourceNode, EdgeGraphics graphics,
                 ArrayList<Command> cmdList, HashMap<TPLTuple<String, BasicNode>, TPLTuple<String, BasicNode>> altStartNodeMap,
                 int probability) {
        super(target, source, targetNode, sourceNode, graphics, cmdList, altStartNodeMap);
        mProbability = probability;
    }

    public int getProbability() {
        return mProbability;
    }

    public void setProbability(int value) {
        mProbability = value;
    }

    public EdgeType getEdgeType() {
        return EdgeType.RandomEdge;
    }

    public String getAbstractSyntax() {
        return null;
    }

    public String getConcreteSyntax() {
        return null;
    }

    public String getFormattedSyntax() {
        return null;
    }

    // TODO:
    public RandomEdge getCopy() {
        return new RandomEdge(mTargetUnid, mSourceUnid, mTargetNode, mSourceNode, mGraphics.getCopy(), getCopyOfCmdList(),
                         getCopyOfAltStartNodeMap(), mProbability);
    }

    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        String   start = "";
        Iterator it    = mAltMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry              pairs            = (Map.Entry) it.next();
            TPLTuple<String, BasicNode> startNodeData    = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodeData = (TPLTuple<String, BasicNode>) pairs.getValue();

            start += startNodeData.getFirst() + "/" + altStartNodeData.getFirst() + ";";
        }

        out.println("<PEdge target=\"" + mTargetUnid + "\" start=\"" + start + "\" probability=\"" + mProbability
                    + "\">").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        if (!mCmdList.isEmpty()) {
            out.println("<Commands>").push();

            for (int i = 0; i < mCmdList.size(); i++) {
                mCmdList.get(i).writeXML(out);
            }

            out.pop().println("</Commands>");
        }

        out.pop().println("</PEdge>");
    }

    public void parseXML(Element element) throws XMLParseError {
        mTargetUnid      = element.getAttribute("target");
        mProbability = Integer.valueOf(element.getAttribute("probability"));

        String[] altStartNodes = element.getAttribute("start").split(";");

        for (String idPair : altStartNodes) {
            if (!idPair.isEmpty()) {
                String[]               ids          = idPair.split("/");
                String                 startId      = ids[0];
                String                 altStartId   = ids[1];
                TPLTuple<String, BasicNode> startPair    = new TPLTuple<String, BasicNode>(startId, null);
                TPLTuple<String, BasicNode> altStartPair = new TPLTuple<String, BasicNode>(altStartId, null);

                mAltMap.put(startPair, altStartPair);
            }
        }

        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                java.lang.String tag = element.getTagName();

                if (tag.equals("Graphics")) {
                    mGraphics = new EdgeGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("Commands")) {
                    XMLParseAction.processChildNodes(element, new XMLParseAction() {
                        @Override
                        public void run(Element element) throws XMLParseError {
                            mCmdList.add(Command.parse(element));
                        }
                    });
                } else {
                    throw new XMLParseError(null,
                                            "Cannot parse an element with tag \"" + tag
                                            + "\" into a child of a PEdge!");
                }
            }
        });
    }
}
