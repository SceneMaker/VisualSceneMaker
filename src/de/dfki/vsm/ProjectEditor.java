package de.dfki.vsm;

import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.File;
import javax.swing.SwingUtilities;

/**
 * @author Gregor Mehlmann
 */
public class ProjectEditor extends DefaultEditor {

    //private final static LOGDefaultLogger sLogger
    //        = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static void main(final String args[]) {
        //
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                // Create A File From Arguments
                final File file = new File(args[0]);
                // Check If File Really Exists
                if (file.exists()) {
                    // Show Project Editor
                    show(new ProjectData(file));
                } else {
                    // Show Default Editor
                    show();
                }
            }
        });
    }
}
