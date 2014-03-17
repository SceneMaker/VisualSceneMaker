package de.dfki.vsm.model.sceneflow.command;

import de.dfki.vsm.model.sceneflow.command.Command.CmdType;
import de.dfki.vsm.util.ios.IndentWriter;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class HistoryDeepClear extends Command {

    private String mState;

    public HistoryDeepClear() {
    }

    public HistoryDeepClear(String state) {
        mState = state;
    }

    public String getState() {
        return mState;
    }

    public CmdType getCmdType() {
        return CmdType.HDC;
    }

    public String getAbstractSyntax() {
        return "HistoryDeepClear ( " + mState + " )";
    }

    public String getConcreteSyntax() {
        return "HistoryDeepClear ( " + mState + " )";
    }

    public String getFormattedSyntax() {
        return "#p#HistoryDeepClear ( " + "#c#" + mState + " )";
    }

    public HistoryDeepClear getCopy() {
        return new HistoryDeepClear(mState);
    }

    public void writeXML(IndentWriter out) {
        out.println("<HistoryDeepClear state=\"" + mState + "\"/>");

    }

    public void parseXML(Element element) {
        mState = element.getAttribute("state");
    }
}
