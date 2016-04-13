/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

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
public class QuestionnaireGUI {

    private JFrame mFrame;
    private QuestionnaireExecutor mExecutor;
    private final Dimension mScreenSize = Toolkit.getDefaultToolkit().getScreenSize();
    // Configurable Values
    private HashMap<String, String> mPersonalValues = new HashMap<>();

    public void init(QuestionnaireExecutor executor, HashMap<String, String> values) {
        mExecutor = executor;
        mPersonalValues = values;

        mFrame = new JFrame("EmpaT User Info");
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
        mFrame.setLocationRelativeTo(null);
        mFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //mFrame.setVisible(true);

        Platform.runLater(() -> initFX(jfxPanel));
    }

    public void setVisible(boolean visible) {
        mFrame.setVisible(visible);
    }

    private void initFX(JFXPanel jfxPanel) {
        FXMLLoader fxmlLoader = new FXMLLoader(getClass().getResource("/res/de/dfki/vsm/xtension/questionnaire/FXMLDocument.fxml"));

        FXMLDocumentController controller = new FXMLDocumentController();
        fxmlLoader.setController(controller);

        try {
            fxmlLoader.load();
        } catch (IOException exception) {
            throw new RuntimeException(exception);
        }

        controller.addListener(mExecutor);

        Parent root = fxmlLoader.getRoot();
        Scene scene = new Scene(root);

        root.setStyle("-fx-background-color: #FFFFFF10;");
        //root.setStyle("-fx-background-color: radial-gradient(focus-distance 0% , center 50% 50% , radius 100% , #9ACD32AA ,#FFFFFF10);"
        //root.setStyle("-fx-effect: dropshadow(three-pass-box, rgba(0,0,0,0.8), 10, 0, 0, 0);");
        
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);

        jfxPanel.setScene(scene);

        // do personalisation
        for (String key : mPersonalValues.keySet()) {
            if (key.equalsIgnoreCase("strength1")) {
                controller.strength1.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength2")) {
                controller.strength2.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength3")) {
                controller.strength3.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength4")) {
                controller.strength4.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength5")) {
                controller.strength5.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("strength6")) {
                controller.strength6.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness1")) {
                controller.weakness1.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness2")) {
                controller.weakness2.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness3")) {
                controller.weakness3.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness4")) {
                controller.weakness4.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness5")) {
                controller.weakness5.setText(mPersonalValues.get(key));
            }
            if (key.equalsIgnoreCase("weakness6")) {
                controller.weakness6.setText(mPersonalValues.get(key));
            }

        }

    }
}
