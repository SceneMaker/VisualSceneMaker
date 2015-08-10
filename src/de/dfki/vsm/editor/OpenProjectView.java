package de.dfki.vsm.editor;

import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.*;

public final class OpenProjectView extends FileView {
    // The Icon To Show For Project Directories
    ImageIcon sceneMakerIcon = new ImageIcon("res/img/icon.png");
    //
    @Override
    public final Icon getIcon(final File file) {
        // The Icon
        Icon icon = null;

        if (file.isDirectory()) {
            File configFile = new File(file.getAbsolutePath() + System.getProperty("file.separator") + "project.xml");

            if (configFile.exists()) {
                icon = sceneMakerIcon;
            }
        }

        return icon;
    }
}
