package de.dfki.vsm.xtension.keyhook.util;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.*;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Patrick, 13.8.2020
 */
public class KeyRunTimePluginProperty implements ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public KeyRunTimePluginProperty(){
        ProjectProperty sceneflowVar = new ProjectProperty("sceneflowVar", true,
                "Name of the global sceneflow variable in which the value of the key is stored in real-time.");
        ProjectValueProperty sceneflowVarVP = new ProjectValueProperty(ValueTYPE.STRING,
                "PressedKey",
                new StringRender());

        exportableProperties.put(sceneflowVar, sceneflowVarVP);
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
