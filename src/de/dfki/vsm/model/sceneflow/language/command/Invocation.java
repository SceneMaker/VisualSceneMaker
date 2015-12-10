package de.dfki.vsm.model.sceneflow.language.command;

import de.dfki.vsm.model.sceneflow.language.command.invocation.UnblockSceneScript;
import de.dfki.vsm.model.sceneflow.language.command.invocation.PlayDialogAct;
import de.dfki.vsm.model.sceneflow.language.command.invocation.PlaySceneGroup;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistorySetDepth;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistoryDeepClear;
import de.dfki.vsm.model.sceneflow.language.command.invocation.HistoryFlatClear;
import de.dfki.vsm.model.sceneflow.language.command.invocation.UnblockSceneGroup;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Invocation extends Command {

    public static Invocation parse(final Element element) throws XMLParseError {
        // The invocation to parse
        Invocation invocation;
        // The name of the XML tag
        final String tag = element.getTagName();

        if (tag.equals("HistoryFlatClear")) {
            invocation = new HistoryFlatClear();
            invocation.parseXML(element);
        } else if (tag.equals("HistoryDeepClear")) {
            invocation = new HistoryDeepClear();
            invocation.parseXML(element);
        } else if (tag.equals("HistorySetDepth")) {
            invocation = new HistorySetDepth();
            invocation.parseXML(element);
        } else if (tag.equals("PlaySceneGroup")) {
            invocation = new PlaySceneGroup();
            invocation.parseXML(element);
        } else if (tag.equals("PlayDialogAct")) {
            invocation = new PlayDialogAct();
            invocation.parseXML(element);
        } else if (tag.equals("UnblockSceneGroup")) {
            invocation = new UnblockSceneGroup();
            invocation.parseXML(element);
        } else if (tag.equals("UnblockSceneScript")) {
            invocation = new UnblockSceneScript();
            invocation.parseXML(element);
        } else {
            // Do nothing and return with NULL
            invocation = null;
        }
        return invocation;
    }

    @Override
    public abstract Invocation getCopy();
}
