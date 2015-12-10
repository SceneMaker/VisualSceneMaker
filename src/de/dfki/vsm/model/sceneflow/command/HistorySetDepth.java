package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command.CmdType;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class HistorySetDepth extends Command {
    private String mState;
    private int    mDepth;

    public HistorySetDepth() {}

    public HistorySetDepth(String state, int depth) {
        mState = state;
        mDepth = depth;
    }

    public int getDepth() {
        return mDepth;
    }

    public String getState() {
        return mState;
    }

    public CmdType getCmdType() {
        return CmdType.HSD;
    }

    public String getAbstractSyntax() {
        return "HistorySetDepth ( " + mState + " , " + mDepth + " )";
    }

    public String getConcreteSyntax() {
        return "HistorySetDepth ( " + mState + " , " + mDepth + " )";
    }

    public String getFormattedSyntax() {
        return "#p#HistorySetDepth ( " + "#c#" + mState + " , " + "#c#" + mDepth + " )";
    }

    public HistorySetDepth getCopy() {
        return new HistorySetDepth(mState, mDepth);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<HistorySetDepth state=\"" + mState + "\" depth=\"" + mDepth + "\"/>");
    }

    public void parseXML(Element element) {
        mState = element.getAttribute("state");
        mDepth = Integer.valueOf(element.getAttribute("depth"));
    }
}
