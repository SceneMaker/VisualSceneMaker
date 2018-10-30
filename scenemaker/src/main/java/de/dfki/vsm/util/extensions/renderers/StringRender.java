package de.dfki.vsm.util.extensions.renderers;

import de.dfki.vsm.util.extensions.value.ValueRenderable;
import javafx.scene.control.TextField;

/**
 * Created by alvaro on 4/20/17.
 */
public class StringRender extends ValueRender {

    private String value;

    @Override
    public void render() {
        value = "";
        setDefaultValue();
        control = new TextField(value);
        setStyle();

    }

    private void setDefaultValue() {
        if(valueProperty.hasDefaultValue()){
            value = (String) valueProperty.getDefaultValue();
        }
    }

    @Override
    public String getValue() {
        return getTextField().getText();
    }

    private TextField getTextField() {
        return (TextField)control;
    }


}
