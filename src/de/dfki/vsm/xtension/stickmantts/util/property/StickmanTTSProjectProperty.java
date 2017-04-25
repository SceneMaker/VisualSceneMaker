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
        ProjectProperty smhost = new ProjectProperty("smhost", true,
                "Host to connect VSM with Stickman.\n " +
                        "If VSM and Stickman are in different computers, this field represents the IP address from " +
                        "the computer where the Stickman is. ");
        ProjectValueProperty smhostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "127.0.0.1",
                new StringRender());

        ProjectProperty smport = new ProjectProperty("smport", true,
                "Port to connect VSM with Stickman.\n " +
                        "This field represents the port where the stickman will be listening to.");
        ProjectValueProperty smportVP= new ProjectValueProperty(ValueTYPE.NUMERIC,
                8000,
                new IntegerRenderer());


        ProjectProperty showstickmanname = new ProjectProperty("showstickmanname",
                "Show the name of the stickman under it.");
        ProjectValueProperty showStickmanNameVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                true,
                new BooleanRenderer());

        ProjectProperty maryBase = new ProjectProperty("mary.base",
                "Path to the MaryTTs folder. \n");
        ProjectValueProperty maryBaseVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                "",
                new FilePathRenderer());
        ProjectProperty fullscreen = new ProjectProperty("fullscreen","If activated, displays the stage at fullscreen");
        ProjectValueProperty fullscreeVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());
        ProjectProperty sticktts = new ProjectProperty("tts", "Specify which TTS library to use.\n" +
                "Be aware that you should supply all the fields related to the selected TTS library");

        ProjectValueProperty stickTTsVP = new ProjectValueProperty(ValueTYPE.LIST,
                "marytts", new SelectableRenderer(), false,
                new ArrayList<>(Arrays.asList("marytts", "cereproc")));

        ProjectProperty character = new ProjectProperty("stickman");
        ProjectValueProperty characterVP = new ProjectValueProperty(ValueTYPE.LIST,
                "Stickman2D", new SelectableRenderer(), false,
                new ArrayList<>(Arrays.asList("StickmanLegacy", "Stickman2D", "Pinocchio", "Reeti")));

        ProjectProperty xStage = new ProjectProperty("xStage", "Initial position of the stage");
        ProjectValueProperty xStageVP = new ProjectValueProperty(ValueTYPE.NUMERIC, 0, new IntegerRenderer());

        ProjectProperty yStage = new ProjectProperty("yStage" , "Initial position of the stage");
        ProjectValueProperty yStageVP = new ProjectValueProperty(ValueTYPE.NUMERIC, 0, new IntegerRenderer());

        ProjectProperty cereprocLicense = new ProjectProperty("license", "Path to the Cereproc license");
        ProjectValueProperty cereprocLicenseVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                "",
                new FilePathRenderer());

        ProjectProperty audioDevice = new ProjectProperty("audio_device");
        ProjectValueProperty audioDeviceVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                "Default",
                new StringRender());

        ProjectProperty cereprocLibPath = new ProjectProperty("cereproc_library_path");
        ProjectValueProperty cereprocLibPathVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                "",
                new FilePathRenderer());




        exportableProperties.put(smhost, smhostVP);
        exportableProperties.put(smport, smportVP);
        exportableProperties.put(showstickmanname, showStickmanNameVP);
        exportableProperties.put(maryBase, maryBaseVP);
        exportableProperties.put(fullscreen, fullscreeVP);
        exportableProperties.put(sticktts, stickTTsVP);
        exportableProperties.put(character, characterVP);
        exportableProperties.put(xStage, xStageVP);
        exportableProperties.put(yStage, yStageVP);
        exportableProperties.put(cereprocLicense, cereprocLicenseVP);
        exportableProperties.put(audioDevice, audioDeviceVP);
        exportableProperties.put(cereprocLibPath, cereprocLibPathVP);


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
