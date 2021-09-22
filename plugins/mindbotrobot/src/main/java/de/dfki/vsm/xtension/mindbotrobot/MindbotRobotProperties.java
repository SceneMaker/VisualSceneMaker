package de.dfki.vsm.xtension.mindbotrobot;

import de.dfki.vsm.extensionAPI.ExportableProperties ;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.StringRender;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

public class MindbotRobotProperties implements ExportableProperties {

    private final Map<ProjectProperty, ProjectValueProperty> mProperties = new HashMap<>();

    public MindbotRobotProperties() {

        mProperties.put(
                new ProjectProperty("rosuri",
                        true,
                        "The URI (e.g., http://localhost:11311) on which the main ROS system and the robot are running."),
                new ProjectValueProperty(ValueTYPE.STRING,
                        "http://localhost:11311",
                        new StringRender(),
                        true
                )
        );

    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return mProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
