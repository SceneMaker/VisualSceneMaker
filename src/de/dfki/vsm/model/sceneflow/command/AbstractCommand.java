package de.dfki.vsm.model.sceneflow.command;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.SyntaxObject;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public abstract class AbstractCommand extends SyntaxObject {
    public enum CmdType {
        PSG, PDA, UASG, USG, ASGN, EXP, HC, HDC, HSD
    }

    public abstract CmdType getCmdType();

    @Override
    public abstract AbstractCommand getCopy();

    public static AbstractCommand parse(Element element) throws XMLParseError {
        AbstractCommand cmd = null;
        String  tag = element.getTagName();

        if (tag.equals("PlaySceneGroup")) {
            cmd = new PlaySceneGroup();
            cmd.parseXML(element);
        } else if (tag.equals("PlayDialogueAct")) {
            cmd = new PlayDialogueAct();
            cmd.parseXML(element);
        } else if (tag.equals("UnblockSceneGroup")) {
            cmd = new UnblockSceneGroup();
            cmd.parseXML(element);
        } else if (tag.equals("UnblockAllSceneGroups")) {
            cmd = new UnblockAllSceneGroups();
            cmd.parseXML(element);
        } else if (tag.equals("Assign")) {
            cmd = new Assignment();
            cmd.parseXML(element);
        } else if (tag.equals("HistoryClear")) {
            cmd = new HistoryClear();
            cmd.parseXML(element);
        } else if (tag.equals("HistoryDeepClear")) {
            cmd = new HistoryDeepClear();
            cmd.parseXML(element);
        } else if (tag.equals("HistorySetDepth")) {
            cmd = new HistorySetDepth();
            cmd.parseXML(element);
        } else {
            cmd = AbstractExpression.parse(element);
        }

        return cmd;
    }
}
