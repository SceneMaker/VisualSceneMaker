package de.dfki.vsm.editor;

//~--- JDK imports ------------------------------------------------------------

import java.io.File;

import javax.swing.*;
import javax.swing.filechooser.*;

public class OpenProjectView extends FileView {
    ImageIcon sceneMakerIcon = new ImageIcon("res/img/icon.png");

    @Override
    public Icon getIcon(File f) {
        Icon icon = null;

        if (f.isDirectory()) {
            File configFile = new File(f.getPath() + System.getProperty("file.separator") + "config.xml");

            if (configFile.exists()) {
                icon = sceneMakerIcon;
            }
        }

        return icon;
    }
}
