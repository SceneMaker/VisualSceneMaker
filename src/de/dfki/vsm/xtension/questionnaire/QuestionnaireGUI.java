/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.IOException;
import java.util.HashMap;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.layout.Region;
import javafx.scene.layout.StackPane;
import javax.swing.JFrame;

/**
 *
 * @author Patrick Gebhard
 */
public class QuestionnaireGUI {

    private FXMLDocumentController mController;
    private Region mRootRegion;
    private Double mScaleFactor = 1.5d;
    // The JavaFX Panel
    final JFXPanel mJFXPanel = new JFXPanel();
    private JFrame mFrame;
    private QuestionnaireExecutor mExecutor;
    // Configurable Values
    private HashMap<String, String> mPersonalValues = new HashMap<>();

    public void init(QuestionnaireExecutor executor, HashMap<String, String> values) {
        mExecutor = executor;
        mPersonalValues = values;

        mFrame = new JFrame("EmpaT User Info");
        mFrame.add(mJFXPanel);

        // Set Not Rezizable
        mFrame.setResizable(false);
        // Set Always On Top
        mFrame.setAlwaysOnTop(true);
        // Set Undecorated
        mFrame.setUndecorated(true);
        // Set Transparent
        mFrame.setBackground(new Color(0, 0, 0, 0));

        mFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        mFrame.setLocationRelativeTo(null);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        Platform.runLater(() -> initFX(mJFXPanel));
    }

    public void setVisible(boolean visible) {
        mFrame.setVisible(visible);
    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtension/questionnaire/FXMLDocument.fxml"));
        mController = new FXMLDocumentController();
        fxmlLoader.setController(mController);
        // add the VSM executor as a listener to the gui controller
        mController.addListener(mExecutor);

        try {
            mRootRegion = (Region) fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }

        // get root
        Group group = new Group(mRootRegion);

        // set root background
        mRootRegion.setStyle("-fx-background-color: #00000010;");

        //scaling
        group.setScaleX(group.getScaleX() * mScaleFactor);
        group.setScaleY(group.getScaleY() * mScaleFactor);

        // place centered
        StackPane rootPane = new StackPane();
        rootPane.getChildren().add(group);

        // set general background, note alpha value must > 0 to ensure modal feature
        rootPane.setStyle("-fx-background-color: #FFFFFF01;");

        // build scene 
        Scene scene = new Scene(rootPane);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        jfxPanel.setScene(scene);

        // do personalisation
        for (String key : mPersonalValues.keySet()) {
            if (key.equalsIgnoreCase("strength1")) {
                mController.strength1.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength2")) {
                mController.strength2.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength3")) {
                mController.strength3.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength4")) {
                mController.strength4.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength5")) {
                mController.strength5.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength6")) {
                mController.strength6.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness1")) {
                mController.weakness1.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness2")) {
                mController.weakness2.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness3")) {
                mController.weakness3.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness4")) {
                mController.weakness4.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness5")) {
                mController.weakness5.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness6")) {
                mController.weakness6.setText(mPersonalValues.get(key));
            }

        }

    }
}
