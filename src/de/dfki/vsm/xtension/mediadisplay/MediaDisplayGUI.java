/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.mediadisplay;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.File;
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
public class MediaDisplayGUI {

    private FXMLDocumentController mController;
    private Region mRootRegion;
    private Double mScaleFactor = 1.5d;

    private JFrame mFrame;
    private MediaDisplayExecutor mExecutor;
    private final Dimension mScreenSize = Toolkit.getDefaultToolkit().getScreenSize();
    // The JavaFX Panel
    final JFXPanel mJFXPanel = new JFXPanel();
    // Configurable Values
    private HashMap<String, String> mDisplayValues = new HashMap<>();
    // The current image
    private String mImageResource;

    public void init(MediaDisplayExecutor executor, HashMap<String, String> values) {
        mExecutor = executor;
        mDisplayValues = values;

        mFrame = new JFrame("EmpaT Media Display");
        mFrame.add(mJFXPanel);

        // Set Not Rezizable
        mFrame.setResizable(false);
        // Set Always On Top
        mFrame.setAlwaysOnTop(true);
        // Set Undecorated
        mFrame.setUndecorated(true);
        // Set Transparent
        mFrame.setBackground(new Color(255, 255, 255, 0));

        mFrame.setExtendedState(JFrame.MAXIMIZED_BOTH);
        mFrame.setLocationRelativeTo(null);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        Platform.runLater(() -> initFX(mJFXPanel));
    }

    public void setImage(String name) {
        if (mDisplayValues.containsKey(name)) {
            mImageResource = "file:///" + mDisplayValues.get("path") + File.separator + mDisplayValues.get(name);
            mImageResource = mImageResource.replace("\\", "/").replace(" ", "%20");

            mController.canvas.setStyle("-fx-background-image: url('" + mImageResource + "'); "
                    + "-fx-background-position: center center; "
                    + "-fx-background-repeat: no-repeat no-repeat;"
                    + "-fx-background-size: contain;"
                    + "-fx-background-color: #00000000;");
        }
    }

    public void setVisible(boolean visible) {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                mFrame.setVisible(visible);
            }
        });

    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtension/mediadisplay/FXMLDocument.fxml"));
        mController = new FXMLDocumentController();
        fxmlLoader.setController(mController);

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
        rootPane.setStyle("-fx-background-color: #FFFFFF00;");

        // build scene 
        Scene scene = new Scene(rootPane);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        jfxPanel.setScene(scene);

        mImageResource = FXMLDocumentController.class.getResource("/res/img/docicon.png").toExternalForm();
        mController.canvas.setStyle("-fx-background-image: url('" + mImageResource + "'); "
                + "-fx-background-position: center center; "
                + "-fx-background-repeat: no-repeat no-repeat;"
                + "-fx-background-size: contain;"
                + "-fx-background-color: #00000000;");
    }
}
