package de.dfki.vsm.xtesting.propertymanager;

import javafx.fxml.FXMLLoader;
import javafx.scene.layout.Pane;

import java.io.IOException;

/**
 * Created by alvaro on 5/26/16.
 */
public class ControlLoader {
    private Pane content = null;

    public ControlLoader(Pane p) {
        try {
            content = (Pane) FXMLLoader.load(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLAddDevice.fxml"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
