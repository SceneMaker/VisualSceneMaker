package de.dfki.vsm.util.extensions.renderers;

import de.dfki.vsm.util.extensions.value.ValueRenderable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;

import java.util.ArrayList;
import java.util.LinkedList;

/**
 * Created by alvaro on 4/20/17.
 */
public class SelectableRenderer extends ValueRender {
    ArrayList<String> values = new ArrayList<>();
    private boolean found;
    private int index;

    @Override
    public void render() {
        found = false;
        values = valueProperty.getOptions();
        ObservableList<String> options = FXCollections.observableArrayList(values);
        findDefaultValue();
        control = new ComboBox(options);
        selectItem();
        found = false;
    }

    private void selectItem() {
        if(found){
            getComboBox().getSelectionModel().select(index);
        }
    }

    private ComboBox getComboBox() {
        return (ComboBox)control;
    }

    @Override
    public String getValue() {
        return (String) getComboBox().getSelectionModel().getSelectedItem();
    }

    private void findDefaultValue() {
        String defaultValue = (String) valueProperty.getDefaultValue();
        if(valueProperty.hasDefaultValue()){
            findValue(defaultValue);
        }
    }

    private void findValue(String defaultValue) {
        index = -1;
        while (index < values.size() && !found){
            index++;
            String value = values.get(index);
            if(value.equals(defaultValue)){
                found = true;
            }

        }
    }


}
