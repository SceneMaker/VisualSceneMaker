package de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers;

import javafx.stage.FileChooser;

import java.io.File;

/**
 * Created by alvaro on 4/26/17.
 */
public class CustomFileChooser implements PathChooser {
    private FileChooser fileChooser;

    public CustomFileChooser(){
        fileChooser = new FileChooser();
    }
    @Override
    public File showDialog() {
        return fileChooser.showOpenDialog(null);
    }
}
