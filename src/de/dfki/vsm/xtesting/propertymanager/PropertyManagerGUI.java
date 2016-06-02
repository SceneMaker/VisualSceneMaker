package de.dfki.vsm.xtesting.propertymanager;

import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtesting.NewPropertyManager.PropertyManagerController;
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
    private  ProjectConfig mConfig;
    private Region mRootRegion;
    private Double mScaleFactor = 1.5d;
    final JFXPanel mJFXPanel = new JFXPanel();
    private JFrame mFrame;
    private StackPane rootPane;
    private RunTimeProject mProject = null;

    public void init(RunTimeProject project){
        try {
            mProject = project;
            mFrame = new JFrame("Property Editor");
            mFrame.add(mJFXPanel);
            FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtesting/NewPropertyManager/PropertyManager.fxml"));

           // Parent root =  FXMLLoader.load(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLDocumentNew.fxml"));
            mController = new PropertyManagerController( mProject);
            fxmlLoader.setController(mController);
            Parent root = fxmlLoader.load();
            Scene scene = new Scene(root);

            mJFXPanel.setScene(scene);
            mJFXPanel.setVisible(true);
            //mFrame.setDefaultCloseOperation(JFrame.);
            mFrame.setVisible(true);
            mFrame.setMinimumSize(new Dimension(800,800));

        } catch (IOException e) {
            e.printStackTrace();
        }

    }
    public void init(ProjectConfig projectConfig, RunTimeProject project){
        mProject = project;
        init(projectConfig);
    }
    public void init(ProjectConfig projectConfig){
        mConfig = projectConfig;
        mFrame = new JFrame("Property Editor");
        mFrame.add(mJFXPanel);

        // Set Not Rezizable
        mFrame.setResizable(false);
        // Set Always On Top
        mFrame.setAlwaysOnTop(true);
        // Set Undecorated
        mFrame.setUndecorated(true);
        // Set Transparent
        mFrame.setBackground(new java.awt.Color(255, 255, 255, 0));
       // mFrame.setBackground(new Color(0, 0, 0, 0));

        mFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        mFrame.setLocationRelativeTo(null);
        mFrame.setSize(600, 400);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        Platform.runLater(() -> initFX());
    }

    public void setVisible(boolean visible) {
       // mFrame.setVisible(visible);
    }

    private void initFX() {
        //FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLDocument.fxml"));
        //mController = new FXMLDocumentController(mConfig, mProject);
       // FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLDocumentNew.fxml"));
        //fxmlLoader.setController(mController);

        /*mFrame = new JFrame("Property editor");
        try {
            mRootRegion = (Region) fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }

        // get root
        Group group = new Group(mRootRegion);

        // set root background
        mRootRegion.setStyle("-fx-background-color: #00000010;");



        // place centered
        rootPane = new StackPane();
        rootPane.getChildren().add(group);

        // set general background, note alpha value must > 0 to ensure modal feature
        rootPane.setStyle("-fx-background-color: #FFFFFF01;");

        // build scene
        //addPlugins();
        Scene scene = new Scene(rootPane);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);

        mJFXPanel.setMinimumSize(new Dimension(600, 400));
        mFrame.setMinimumSize(new Dimension(600, 400));

        mJFXPanel.setScene(scene);
        mJFXPanel.setVisible(true);*/

    }

    private void addPlugins(){
        /*for (PluginConfig plugin: mConfig.getPluginConfigList() ) {

            Label label = new Label("Plugin");
            final VBox vbox = new VBox();
            vbox.setSpacing(5);
            //vbox.setPadding(new Insets(10, 0, 0, 10));
            //TableView table = getTable(plugin);
            //vbox.getChildren().addAll(label, table);
            rootPane.getChildren().add(vbox);

        }*/
    }

    /*private TableView getTable(PluginConfig plugin){
        ObservableList<ConfigFeature> data = FXCollections.observableArrayList();
        TableView table = new TableView();
        table.setEditable(true);
        TableColumn keyCol = new TableColumn("Key");
        TableColumn ValueCol = new TableColumn("Value");
        table.getColumns().addAll(keyCol, ValueCol);
        for (ConfigFeature feat :plugin.getEntryList() ) {
            data.add(feat);
        }
        ValueCol.setEditable(true);

        keyCol.setCellValueFactory( new PropertyValueFactory<ConfigFeature, String>("key")) ;
        ValueCol.setCellValueFactory( new PropertyValueFactory<ConfigFeature, String>("value")) ;
        table.setItems(data);
        return table;
    }*/
}
