package de.dfki.vsm.xtension.stickmantts.util.property;

import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.renderers.*;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.extensions.value.ValueTYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Created by alvaro on 4/20/17.
 */
public class StickmanTTSProjectProperty implements ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public StickmanTTSProjectProperty(){
        ProjectProperty smhost = new ProjectProperty("smhost", true);
        ProjectValueProperty smhostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "127.0.0.1",
                new StringRender());

        ProjectProperty smport = new ProjectProperty("smport", true);
        ProjectValueProperty smportVP= new ProjectValueProperty(ValueTYPE.NUMERIC,
                8000,
                new IntegerRenderer());


        ProjectProperty showstickmanname = new ProjectProperty("showstickmanname");
        ProjectValueProperty showStickmanNameVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                true,
                new BooleanRenderer());

        ProjectProperty maryBase = new ProjectProperty("mary.base");
        ProjectValueProperty maryBaseVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                "",
                new FilePathRenderer());
        ProjectProperty fullscreen = new ProjectProperty("fullscreen");
        ProjectValueProperty fullscreeVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());
        ProjectProperty sticktts = new ProjectProperty("tts");

        ProjectValueProperty stickTTsVP = new ProjectValueProperty(ValueTYPE.LIST,
                "marytts", new SelectableRenderer(), false,
                new ArrayList<>(Arrays.asList("marytts", "cereproc")));

        ProjectProperty character = new ProjectProperty("stickman");
        ProjectValueProperty characterVP = new ProjectValueProperty(ValueTYPE.LIST,
                "Stickman2D", new SelectableRenderer(), false,
                new ArrayList<>(Arrays.asList("StickmanLegacy", "Stickman2D", "Pinocchio", "Reeti")));

        exportableProperties.put(smhost, smhostVP);
        exportableProperties.put(smport, smportVP);
        exportableProperties.put(showstickmanname, showStickmanNameVP);
        exportableProperties.put(maryBase, maryBaseVP);
        exportableProperties.put(fullscreen, fullscreeVP);
        exportableProperties.put(sticktts, stickTTsVP);
        exportableProperties.put(character, characterVP);

    }

    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties;
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        //TODO: Implement
        return null;

    }
}
