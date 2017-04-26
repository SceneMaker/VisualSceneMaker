package de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers;

import javafx.stage.DirectoryChooser;

import java.io.File;

/**
 * Created by alvaro on 4/26/17.
 */
public class CustomDirectoryChooser implements PathChooser {
    private DirectoryChooser directoryChooser;

    public CustomDirectoryChooser(){
        directoryChooser = new DirectoryChooser();
    }

    @Override
    public File showDialog() {
        return directoryChooser.showDialog(null);
    }
}
