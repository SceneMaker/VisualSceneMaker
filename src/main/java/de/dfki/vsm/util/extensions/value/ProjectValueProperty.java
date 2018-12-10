package de.dfki.vsm.util.extensions.value;

import java.util.ArrayList;

/**
 * Created by alvaro on 4/20/17.
 */
public class ProjectValueProperty implements PropertyValue {
    private final boolean required;
    private final ArrayList<String> options;
    ValueTYPE type;
    Object defaultValue;
    ValueRenderable renderer;
    private String value;


    public ProjectValueProperty(ValueTYPE type, Object defaultValue, ValueRenderable renderer){
        this.type = type;
        this.defaultValue = defaultValue;
        this.renderer = renderer;
        this.required = false;
        this.options = new ArrayList<>();
        this.renderer.setValueProperty(this);
    }

    public ProjectValueProperty(ValueTYPE type, Object defaultValue, ValueRenderable renderer, boolean required){
        this.type = type;
        this.defaultValue = defaultValue;
        this.renderer = renderer;
        this.required = required;
        this.options = new ArrayList<>();
        this.renderer.setValueProperty(this);
    }

    public ProjectValueProperty(ValueTYPE type, Object defaultValue, ValueRenderable renderer, boolean required,
                                ArrayList<String> options){
        this.type = type;
        this.defaultValue = defaultValue;
        this.renderer = renderer;
        this.required = required;
        this.renderer.setValueProperty(this);
        this.options = options;
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

    public ValueRenderable getRenderer() {
        return renderer;
    }

    public void setRenderer(ValueRenderable renderer) {
        this.renderer = renderer;
    }

    @Override
    public void render() {
        renderer.render();
    }

    @Override
    public boolean validate() {
        return true;
    }

    public String getValue() {
        value = renderer.getValue();
        return value;
    }

    public boolean hasDefaultValue(){
        return defaultValue != null;
    }

    public ArrayList<String> getOptions(){
        return options;
    }


}
