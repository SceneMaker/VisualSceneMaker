package de.dfki.vsm.util.extensions;

import de.dfki.vsm.util.extensions.value.ProjectValueProperty;

import java.util.HashMap;

/**
 * Created by alvaro on 4/20/17.
 */
public interface ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties();
    HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties();
}
