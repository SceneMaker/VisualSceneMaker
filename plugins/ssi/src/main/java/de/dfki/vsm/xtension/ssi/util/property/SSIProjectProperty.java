package de.dfki.vsm.xtension.ssi.util.property;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.BooleanRenderer;
import de.dfki.vsm.extensionAPI.renderers.IntegerRenderer;
import de.dfki.vsm.extensionAPI.renderers.StringRender;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Patrick Gebhard
 */
public class SSIProjectProperty implements ExportableProperties {

    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public SSIProjectProperty() {

        ProjectProperty loghost = new ProjectProperty("loghost", true,
                "Host that runs the SSI.\n "
                + "If SSI runs on a different computer, this field should hold the IP address of "
                + "the computer");
        ProjectValueProperty loghostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "192.168.0.105",
                new StringRender());

        ProjectProperty logport = new ProjectProperty("logport", true,
                "Port to connect to SSI.\n "
                + "This field holds the SSI listening port.");
        ProjectValueProperty logportVP = new ProjectValueProperty(ValueTYPE.NUMERIC,
                8989,
                new IntegerRenderer());

        ProjectProperty logvar = new ProjectProperty("logvar", true,
                "A VSM Sceneflow var that gets messages from SSI");
        ProjectValueProperty logvarVP = new ProjectValueProperty(ValueTYPE.STRING,
                "SSIMessages",
                new StringRender());

        ProjectProperty pipes = new ProjectProperty("pipes", true,
                "SSI recognition pipeline description");
        ProjectValueProperty pipesVP = new ProjectValueProperty(ValueTYPE.STRING,
                "interview:127.0.0.1:1111",
                new StringRender());

        ProjectProperty broadcast = new ProjectProperty("broadcast",
                "Broadcast messages in local network. Set to \"true\" for StudyMaster remote control");
        ProjectValueProperty broadcastVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());

        ProjectProperty broadcastport = new ProjectProperty("broadcastport",
                "Port on which messages are broadcasted.");
        ProjectValueProperty broadcastportVP = new ProjectValueProperty(ValueTYPE.NUMERIC,
                9898,
                new IntegerRenderer());

        ProjectProperty listentostudymaster = new ProjectProperty("listentostudymaster",
                "Listen to StudyMaster input. Set to \"true\" for StudyMaster remote control");
        ProjectValueProperty listentostudymasterVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());

        ProjectProperty studymasterport = new ProjectProperty("broadcastport",
                "Port on which StudyMaster receives messages.");
        ProjectValueProperty studymasterportVP = new ProjectValueProperty(ValueTYPE.NUMERIC,
                9898,
                new IntegerRenderer());

        exportableProperties.put(loghost, loghostVP);
        exportableProperties.put(logport, logportVP);
        exportableProperties.put(logvar, logvarVP);
        exportableProperties.put(pipes, pipesVP);
        exportableProperties.put(broadcast, broadcastVP);
        exportableProperties.put(broadcastport, broadcastportVP);
        exportableProperties.put(listentostudymaster, listentostudymasterVP);
        exportableProperties.put(studymasterport, studymasterportVP);
    }

    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        //TODO: Implement
        return null;

    }
}
