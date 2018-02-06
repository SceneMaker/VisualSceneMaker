package de.dfki.vsm.xtension.decad.properties;

import de.dfki.vsm.util.extensions.ExportableCompletion;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtension.decad.commands.AnimationsListCommand;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import org.jetbrains.annotations.NotNull;
import org.json.JSONArray;

import java.io.IOException;
import java.util.ArrayList;

public class ExportAnimations implements ExportableCompletion {
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    @Override
    public ArrayList<String> getExportableActions() {
        try {
            return getAnimationListFromServer();
        } catch (InterruptedException | IOException e) {
            mLogger.message("Could not load animations from server");
        }
        return new ArrayList<>();
    }

    private ArrayList<String> getAnimationListFromServer() throws InterruptedException, IOException {
        JSONArray jsonAnimation = fetchAnimationsFromServer();
        return convertJsonArrayToArrayList(jsonAnimation);
    }

    @NotNull
    private ArrayList<String> convertJsonArrayToArrayList(JSONArray jsonAnimation) {
        ArrayList<String> animations = new ArrayList<>();
        for (int i = 0; i < jsonAnimation.length(); i++) {
            animations.add(jsonAnimation.getString(i));
        }
        return animations;
    }

    @NotNull
    private JSONArray fetchAnimationsFromServer() throws IOException, InterruptedException {
        DecadCommand animationList = new AnimationsListCommand();
        animationList.execute();
        String animationsResponse = animationList.getResponse();
        return new JSONArray(animationsResponse);
    }


}
