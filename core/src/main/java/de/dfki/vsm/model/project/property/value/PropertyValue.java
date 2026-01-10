package de.dfki.vsm.model.project.property.value;

/**
 * Marker for a property value with basic validation.
 */
public interface PropertyValue {
    boolean validate();

    String getValue();
}
