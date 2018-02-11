package de.dfki.vsm.xtension.stickman.util.property;

import de.dfki.common.interfaces.StageRoom;
import de.dfki.vsm.util.extensions.ExportableCompletion;
import de.dfki.vsm.util.stickman.StickmanRepository;

import java.util.List;

public class StickmanActions implements ExportableCompletion{
    private final StickmanRepository stickmanFactory;

    public StickmanActions(StickmanRepository stickmanFactory) {
        this.stickmanFactory = stickmanFactory;
    }

    @Override
    public List<String> getExportableActions() {
        StageRoom stickmanStageC = stickmanFactory.createStickman();
        return stickmanStageC.getAnimations();
    }
}
