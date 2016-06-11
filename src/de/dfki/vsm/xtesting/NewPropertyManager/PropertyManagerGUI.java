package de.dfki.vsm.xtesting.NewPropertyManager;

import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;

/**
 * Created by alvaro on 4/23/16.
 */
public class PropertyManagerGUI {
    private PropertyManagerController mController;
    private ProjectConfig mConfig;

    private JFrame mFrame;
    private RunTimeProject mProject = null;
    private final Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
    public void init(RunTimeProject project){
        mProject = project;

        init(project.getProjectConfig());
    }
    public void init(ProjectConfig projectConfig, RunTimeProject project){
        mProject = project;
        init(projectConfig);
    }
    public void init(ProjectConfig projectConfig){

        mConfig = projectConfig;
        mFrame = new JFrame("Property Editor");
        mFrame  .setLayout(new BorderLayout());
        final JFXPanel mJFXPanel = new JFXPanel();


        // Set Always On Top
        mFrame.add(mJFXPanel,BorderLayout.CENTER);
        mFrame.setAlwaysOnTop(true);
        // Set Undecorated
        mFrame.setUndecorated(false);

        mFrame.setLocationRelativeTo(null);
        int width = (int) (dim.getWidth() * 0.60);
        int height = (int) (dim.getHeight() * 0.70);
        mFrame.setSize(width, height);
       // mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        mFrame.setVisible(true);


        mFrame.setLocation(dim.width/2-mFrame.getSize().width/2, dim.height/2-mFrame.getSize().height/2);
        Platform.runLater(() -> initFX(mJFXPanel));

    }

    public void setVisible(boolean visible) {
       // mFrame.setVisible(visible);
    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtesting/NewPropertyManager/PropertyManager.fxml"));

        // Parent root =  FXMLLoader.load(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLDocumentNew.fxml"));
        mController = new PropertyManagerController( mProject);
        fxmlLoader.setController(mController);
        Parent root = null;
        try {
            root = fxmlLoader.load();
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }
        Scene scene = new Scene(root);
        jfxPanel.setScene(scene);
        //jfxPanel.setVisible(true);
        //mFrame.setVisible(true);


    }




}
