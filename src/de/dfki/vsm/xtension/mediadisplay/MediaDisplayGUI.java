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

    public void init(MediaDisplayExecutor executor) {
        mExecutor = executor;

        mFrame = new JFrame("EmpaT Media Display");
        mFrame.setLayout(new BorderLayout());
        final JFXPanel jfxPanel = new JFXPanel();
        mFrame.add(jfxPanel, BorderLayout.CENTER);

        Dimension sideBorder = new Dimension((mScreenSize.width - 800) / 2, 200);
        Dimension topdownBorder = new Dimension(200, (mScreenSize.height - 600) / 2);

        JPanel mWestPanel = new JPanel();
        mWestPanel.setLayout(new BoxLayout(mWestPanel, BoxLayout.X_AXIS));
        mWestPanel.setBorder(null);
        mWestPanel.setBackground(new Color(0, 0, 0, 1));
        mWestPanel.setMinimumSize(sideBorder);
        mWestPanel.setMaximumSize(sideBorder);
        mWestPanel.setPreferredSize(sideBorder);
        mFrame.add(mWestPanel, BorderLayout.WEST);

        JPanel mNorthPanel = new JPanel();
        mNorthPanel.setLayout(new BoxLayout(mNorthPanel, BoxLayout.X_AXIS));
        mNorthPanel.setBorder(null);
        mNorthPanel.setBackground(new Color(0, 0, 0, 1));
        mNorthPanel.setMinimumSize(topdownBorder);
        mNorthPanel.setMaximumSize(topdownBorder);
        mNorthPanel.setPreferredSize(topdownBorder);
        mFrame.add(mNorthPanel, BorderLayout.NORTH);

        JPanel mSouthPanel = new JPanel();
        mSouthPanel.setLayout(new BoxLayout(mSouthPanel, BoxLayout.X_AXIS));
        mSouthPanel.setBorder(null);
        mSouthPanel.setBackground(new Color(0, 0, 0, 1));
        mSouthPanel.setMinimumSize(topdownBorder);
        mSouthPanel.setMaximumSize(topdownBorder);
        mSouthPanel.setPreferredSize(topdownBorder);
        mFrame.add(mSouthPanel, BorderLayout.SOUTH);

        JPanel mEastPanel = new JPanel();
        mEastPanel.setLayout(new BoxLayout(mEastPanel, BoxLayout.X_AXIS));
        mEastPanel.setBorder(null);
        mEastPanel.setBackground(new Color(0, 0, 0, 1));
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
        mFrame.setBackground(new Color(0, 0, 0, 0));

        mFrame.setSize(mScreenSize);
        //mFrame.setSize(800, 600);
        mFrame.setLocationRelativeTo(null);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //mFrame.setVisible(true);

        Platform.runLater(() -> initFX(jfxPanel));
    }

    public void setVisible(boolean visible) {
        mFrame.setVisible(visible);
    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtension/mediadisplay/FXMLDocument.fxml"));

        FXMLDocumentController controller = new FXMLDocumentController();
        fxmlLoader.setController(controller);

        try {
            fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }

        Parent root = fxmlLoader.getRoot();
        Scene scene = new Scene(root);
        
        root.setStyle("-fx-background-color: #00000000;");
        
        //scene.setFill(new javafx.scene.paint.Color(255,0,0,255));
        
        
        jfxPanel.setScene(scene);

        String image = FXMLDocumentController.class.getResource("/res/img/docicon.png").toExternalForm();

        controller.canvas.setStyle("-fx-background-image: url('" + image + "'); "
                + "-fx-background-position: center center; "
                + "-fx-background-repeat: no-repeat no-repeat;"
                + "-fx-background-size: contain;"
                + "-fx-background-color: #00000000;");
    }
}
