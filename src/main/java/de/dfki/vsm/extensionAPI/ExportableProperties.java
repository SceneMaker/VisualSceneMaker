package de.dfki.vsm.extensionAPI;

import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;

import java.util.Map;

/**
 * Created by alvaro on 4/20/17.
 */
public interface ExportableProperties {
    Map<ProjectProperty, ProjectValueProperty> getExportableProperties();

    Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties();
}
