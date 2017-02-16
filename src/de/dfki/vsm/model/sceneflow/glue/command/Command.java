package de.dfki.vsm.model.sceneflow.glue.command;

import de.dfki.vsm.model.sceneflow.glue.SyntaxObject;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Command extends SyntaxObject {

    @Override
    public abstract Command getCopy();

    public static Command parse(final Element element) throws XMLParseError {
        // The command to parse
        Command command = null;
        // The name of the XML tag
        final String tag = element.getTagName();
        // Parse the command
        if (tag.equals("Assignment")) {
            command = new Assignment();
            command.parseXML(element);
        } else {
            // Try to parse expression
            command = Expression.parse(element);
            if (command == null) {
                // Try to parse invocation
                command = Invocation.parse(element);
                if (command == null) {
                    // Try to parse invocation
                    command = Definition.parse(element);
                }
            }
        }
        return command;
        /*
        else if (tag.equals("PlaySceneGroup")) {
            command = new PlaySceneGroup();
            command.parseXML(element);
        } else if (tag.equals("PlayActionCommand")) {
            command = new PlayActionCommand();
            command.parseXML(element);
        } else if (tag.equals("PlayActionSequential")) {
            command = new PlayActionSequential();
            command.parseXML(element);
        } else if (tag.equals("PlayActionConcurrent")) {
            command = new PlayActionConcurrent();
            command.parseXML(element);
        } else if (tag.equals("PlayDialogAct")) {
            command = new PlayDialogAction();
            command.parseXML(element);
        } else if (tag.equals("FreeOneSceneGroup")) {
            command = new FreeOneSceneGroup();
            command.parseXML(element);
        } else if (tag.equals("FreeAllSceneGroups")) {
            command = new FreeAllSceneGroups();
            command.parseXML(element);
        } else if (tag.equals("HistoryClearFlat")) {
            command = new HistoryClearFlat();
            command.parseXML(element);
        } else if (tag.equals("HistoryClearDeep")) {
            command = new HistoryClearDeep();
            command.parseXML(element);
        } else if (tag.equals("HistorySetDepth")) {
            command = new HistorySetDepth();
            command.parseXML(element);
        } else {
            command = Expression.parse(element);
        }
        return command;
         */
    }
}
