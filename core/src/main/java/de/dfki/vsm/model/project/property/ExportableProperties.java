package de.dfki.vsm.model.project.property;

import de.dfki.vsm.model.project.property.value.ProjectValueProperty;

import java.util.Map;

/**
 * Declares project-level properties that can be exported/edited.
 */
public interface ExportableProperties {
    Map<ProjectProperty, ProjectValueProperty> getExportableProperties();

    Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties();
}
