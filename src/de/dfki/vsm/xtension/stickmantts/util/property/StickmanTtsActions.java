package de.dfki.vsm.xtension.stickmantts.util.property;

import de.dfki.vsm.util.extensions.ExportableCompletion;

import java.util.ArrayList;

public class StickmanTtsActions implements ExportableCompletion{
    @Override
    public ArrayList<String> getExportableActions() {
        //TODO: Take from stickman.jar
        ArrayList<String> actions = new ArrayList<>();
        actions.add("Smile");
        actions.add("Sad");
        actions.add("Contempt");
        actions.add("Happy");
        return actions;
    }
}
