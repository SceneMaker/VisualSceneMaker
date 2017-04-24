package de.dfki.vsm.util.extensions.renderers;

import javafx.scene.control.CheckBox;

/**
 * Created by alvaro on 4/20/17.
 */
public class BooleanRenderer extends ValueRender {

    public static final String TRUE = "true";
    public static final String FALSE = "false";
    private Boolean value;
    @Override
    public void render() {
        value = false;
        setDefault();
        control = new CheckBox();
        getCheckbox().setSelected(value);
        setStyle();
    }

    private void setDefault() {
        if(valueProperty.hasDefaultValue()){
            value = (Boolean) valueProperty.getDefaultValue();
        }
    }

    private CheckBox getCheckbox() {
        return (CheckBox)control;
    }

    @Override
    public String getValue() {
        if(getCheckbox().isSelected())
            return TRUE;
        return FALSE;
    }

    @Override
    public void setStyle(){

    }


}
