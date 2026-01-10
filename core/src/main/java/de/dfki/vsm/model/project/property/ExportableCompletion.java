package de.dfki.vsm.model.project.property;

import java.util.List;

/**
 * Completion hints that can be exported for commands/fields.
 */
public interface ExportableCompletion {
    List<String> getExportableActions();
}
