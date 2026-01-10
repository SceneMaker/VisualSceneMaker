package de.dfki.vsm.model.project.property.value;

import java.util.ArrayList;

/**
 * Property value definition (type, default, required, options).
 */
public class ProjectValueProperty implements PropertyValue {
    private final boolean required;
    private final ArrayList<String> options;
    ValueTYPE type;
    Object defaultValue;
    private String value;

    public ProjectValueProperty(ValueTYPE type, Object defaultValue) {
        this(type, defaultValue, false, new ArrayList<>());
    }

    public ProjectValueProperty(ValueTYPE type, Object defaultValue, boolean required) {
        this(type, defaultValue, required, new ArrayList<>());
    }

    public ProjectValueProperty(ValueTYPE type, Object defaultValue, boolean required, ArrayList<String> options) {
        this.type = type;
        this.defaultValue = defaultValue;
        this.required = required;
        this.options = options != null ? options : new ArrayList<>();
    }

    public ValueTYPE getType() {
        return type;
    }

    public void setType(ValueTYPE type) {
        this.type = type;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    @Override
    public boolean validate() {
        return true;
    }

    public String getValue() {
        if (value != null) {
            return value;
        }
        if (defaultValue == null) {
            return null;
        }
        return String.valueOf(defaultValue);
    }

    public void setValue(String value) {
        this.value = value;
    }

    public boolean hasDefaultValue() {
        return defaultValue != null;
    }

    public ArrayList<String> getOptions() {
        return options;
    }
}
