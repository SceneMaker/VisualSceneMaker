package de.dfki.vsm.xtension.stickmantts.util.property;

import de.dfki.common.interfaces.StageRoom;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.util.extensions.ExportableCompletion;
import de.dfki.vsm.util.stickman.StickmanRepository;

import java.util.ArrayList;

public class StickmanTtsActions implements ExportableCompletion{
    private final PluginConfig config;
    private final StickmanRepository stickmanFactory;

    public StickmanTtsActions(PluginConfig config, StickmanRepository stickmanFactory) {
        this.config = config;
        this.stickmanFactory = stickmanFactory;
    }

    @Override
    public ArrayList<String> getExportableActions() {
        StageRoom stickmanStageC = stickmanFactory.createStickman();
        return stickmanStageC.getAnimations();
    }
}
