package de.dfki.vsm.xtension.keyhook.util;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.*;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

/**
 * Patrick, 13.8.2020
 */
public class KeyRunTimePluginProperty implements ExportableProperties {

    public static final String PRESSED_KEY_VAR_NAME = "sceneflowVar";
    public static final String PRESSED_KEY_VAR_DEFAULT = "PressedKey" ;

    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public KeyRunTimePluginProperty(){
        ProjectProperty sceneflowVar = new ProjectProperty(PRESSED_KEY_VAR_NAME, true,
                "Name of the global sceneflow variable in which the value of the key is stored in real-time.");
        ProjectValueProperty sceneflowVarVP = new ProjectValueProperty(ValueTYPE.STRING,
                PRESSED_KEY_VAR_DEFAULT,
                new StringRender());

        exportableProperties.put(sceneflowVar, sceneflowVarVP);
    }

    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
