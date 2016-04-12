/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.mediadisplay;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 *
 * @author Patrick Gebhard
 */
public class MediaDisplayGUI {

    private JFrame mFrame;
    private MediaDisplayExecutor mExecutor;
    private final Dimension mScreenSize = Toolkit.getDefaultToolkit().getScreenSize();
    // The JavaFX Panel
    final JFXPanel mJFXPanel = new JFXPanel();
    FXMLDocumentController mController = new FXMLDocumentController();
    // Configurable Values
    private HashMap<String, String> mDisplayValues = new HashMap<>();
    // The current image
    private String mImageResource;

    public void init(MediaDisplayExecutor executor, HashMap<String, String> values) {
        mExecutor = executor;
        mDisplayValues = values;

        mFrame = new JFrame("EmpaT Media Display");
        mFrame.setLayout(new BorderLayout());
        mFrame.add(mJFXPanel, BorderLayout.CENTER);

        Dimension sideBorder = new Dimension((mScreenSize.width - 800) / 2, 200);
        Dimension topdownBorder = new Dimension(200, (mScreenSize.height - 600) / 2);

        JPanel mWestPanel = new JPanel();
        mWestPanel.setLayout(new BoxLayout(mWestPanel, BoxLayout.X_AXIS));
        mWestPanel.setBorder(null);
        mWestPanel.setBackground(new Color(255, 255, 255, 1));
        mWestPanel.setMinimumSize(sideBorder);
        mWestPanel.setMaximumSize(sideBorder);
        mWestPanel.setPreferredSize(sideBorder);
        mFrame.add(mWestPanel, BorderLayout.WEST);

        JPanel mNorthPanel = new JPanel();
        mNorthPanel.setLayout(new BoxLayout(mNorthPanel, BoxLayout.X_AXIS));
        mNorthPanel.setBorder(null);
        mNorthPanel.setBackground(new Color(255, 255, 255, 1));
        mNorthPanel.setMinimumSize(topdownBorder);
        mNorthPanel.setMaximumSize(topdownBorder);
        mNorthPanel.setPreferredSize(topdownBorder);
        mFrame.add(mNorthPanel, BorderLayout.NORTH);

        JPanel mSouthPanel = new JPanel();
        mSouthPanel.setLayout(new BoxLayout(mSouthPanel, BoxLayout.X_AXIS));
        mSouthPanel.setBorder(null);
        mSouthPanel.setBackground(new Color(255, 255, 255, 1));
        mSouthPanel.setMinimumSize(topdownBorder);
        mSouthPanel.setMaximumSize(topdownBorder);
        mSouthPanel.setPreferredSize(topdownBorder);
        mFrame.add(mSouthPanel, BorderLayout.SOUTH);

        JPanel mEastPanel = new JPanel();
        mEastPanel.setLayout(new BoxLayout(mEastPanel, BoxLayout.X_AXIS));
        mEastPanel.setBorder(null);
        mEastPanel.setBackground(new Color(255, 255, 255, 1));
        mEastPanel.setMinimumSize(sideBorder);
        mEastPanel.setMaximumSize(sideBorder);
        mEastPanel.setPreferredSize(sideBorder);
        mFrame.add(mEastPanel, BorderLayout.EAST);

        // Set Not Rezizable
        mFrame.setResizable(false);
        // Set Always On Top
        mFrame.setAlwaysOnTop(true);
        // Set Undecorated
        mFrame.setUndecorated(true);
        // Set Transparent
        mFrame.setBackground(new Color(255, 255, 255, 0));

        mFrame.setSize(mScreenSize);
        //mFrame.setSize(800, 600);
        mFrame.setLocationRelativeTo(null);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        Platform.runLater(() -> initFX(mJFXPanel));
    }

    public void setImage(String name) {
        if (mDisplayValues.containsKey(name)) {
            mImageResource = "file:///" + mDisplayValues.get("path")  + File.separator + mDisplayValues.get(name);
            mImageResource = mImageResource.replace("\\", "/").replace(" ", "%20");

            mController.canvas.setStyle("-fx-background-image: url('" + mImageResource + "'); "
                    + "-fx-background-position: center center; "
                    + "-fx-background-repeat: no-repeat no-repeat;"
                    + "-fx-background-size: contain;"
                    + "-fx-background-color: #00000000;");
        }
    }

    public void setVisible(boolean visible) {
        mFrame.setVisible(visible);
    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtension/mediadisplay/FXMLDocument.fxml"));
        fxmlLoader.setController(mController);

        try {
            fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }

        Parent root = fxmlLoader.getRoot();
        Scene scene = new Scene(root);

        root.setStyle("-fx-background-color: null;");
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
