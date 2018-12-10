package de.dfki.vsm.xtesting;

import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.ScriptParser;

/**
 * @author Gregor Mehlmann
 */
public class TestParser {

    public static void main(String args[]) {
        System.err.println("\" Hi how are you today?\"");
        final String action = "actor: action key='val'";
        // Parse Content Into Scene Script
        final ActionObject result = (ActionObject)ScriptParser.run(action, true, false, true, false, false);
        System.err.println(result);
    }
}
