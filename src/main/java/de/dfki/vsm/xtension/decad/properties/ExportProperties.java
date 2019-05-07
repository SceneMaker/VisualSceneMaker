package de.dfki.vsm.xtension.decad.properties;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.IntegerRenderer;
import de.dfki.vsm.extensionAPI.renderers.StringRender;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

public class ExportProperties implements ExportableProperties {
    private final HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        ProjectProperty url = new ProjectProperty("url", true,
                "Url to connect VSM with DECAD.\n ");
        ProjectValueProperty urlVP = new ProjectValueProperty(ValueTYPE.STRING,
                "http://localhost",
                new StringRender());

        ProjectProperty port = new ProjectProperty("port", true,
                "Port to connect VSM with DECAD.\n " +
                        "This field represents the port of the web server.");
        ProjectValueProperty portVP = new ProjectValueProperty(ValueTYPE.NUMERIC,
                5005,
                new IntegerRenderer());

        exportableProperties.put(url, urlVP);
        exportableProperties.put(port, portVP);
        return exportableProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return new HashMap<>();
    }
}
