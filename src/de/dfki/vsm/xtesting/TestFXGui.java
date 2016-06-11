package de.dfki.vsm.xtesting;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtesting.NewPropertyManager.PropertyManagerGUI;

import javax.swing.*;

/**
 * @author Not me
 */
public class TestFXGui {

    // Get The System logger
    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    public static void main(String args[]) {
        PropertyManagerGUI gui = new PropertyManagerGUI();
        RunTimeProject project = new RunTimeProject();
        //gui.init();
        //gui.setVisible(true);
        String testPath = "res/tutorials/2-EmpaT";
        project.parseForInformation(testPath);
        if(project!= null && project.getProjectConfig()!= null) {
            System.out.println("ENTRA MAIN----------------");
            //gui.init(project);
            SwingUtilities.invokeLater(() -> gui.init(project));
            //gui.setVisible(true);

        }
        else {
            System.out.println("Error reading the project");
        }
    }

    public void runGui(){

    }

}
