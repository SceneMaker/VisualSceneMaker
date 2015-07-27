package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command.CmdType;
import de.dfki.vsm.util.ios.IOSIndentWriter;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public class HistoryClear extends Command {
    private String mState;

    public HistoryClear() {}

    public HistoryClear(String state) {
        mState = state;
    }

    public String getState() {
        return mState;
    }

    public CmdType getCmdType() {
        return CmdType.HC;
    }

    public String getAbstractSyntax() {
        return "HistoryClear ( " + mState + " )";
    }

    public String getConcreteSyntax() {
        return "HistoryClear ( " + mState + " )";
    }

    public String getFormattedSyntax() {
        return "#p#HistoryClear ( " + "#c#" + mState + " )";
    }

    public HistoryClear getCopy() {
        return new HistoryClear(mState);
    }

    public void writeXML(IOSIndentWriter out) {
        out.println("<HistoryClear state=\"" + mState + "\"/>");
    }

    public void parseXML(Element element) {
        mState = element.getAttribute("state");
    }
}
