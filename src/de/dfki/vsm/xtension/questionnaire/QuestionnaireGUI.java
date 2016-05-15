/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.awt.Color;
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
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    
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
        
        Platform.runLater(() -> initFX());
    }
    
    public void setVisible(boolean visible) {
        mFrame.setVisible(visible);
    }

    // interface for name
    public void setName(String name) {
        mLogger.message("Got " + name);
        
        mController.namefield.setText(Character.toUpperCase(name.charAt(0)) + name.substring(1));
        mController.ageslider.requestFocus();
        mController.agefield.setText("20");
        mController.nextbutton.setDisable(false);
        mFrame.repaint();
    }

    // interface for age
    public void setAge(String age) {
        mLogger.message("Got " + age);
        
        mController.ageslider.setValue(Double.parseDouble(age) - 20);
        mController.agefield.setText(age);
        mController.sexfemale.requestFocus();
        mFrame.repaint();
    }

    // interface for sex
    public void setSex(String sex) {
        mLogger.message("Got " + sex);
        
        if (sex.equalsIgnoreCase("female")) {
            mController.sexfemale.setSelected(true);
        } else {
            mController.sexmale.setSelected(true);
        }
        mController.jobinterviewslider.requestFocus();
        mController.jobvinterviewfield.setText("keine");
        mFrame.repaint();
    }

    // interface for job interviews
    public void setJobinterviews(String interviews) {
        mLogger.message("Got " + interviews);
        
        mController.jobinterviewslider.setValue(Double.parseDouble(interviews));
        mController.jobvinterviewfield.setText((interviews.equalsIgnoreCase("none")) ? "keine" : (Integer.parseInt(interviews) > 8) ? ">8" : interviews);
        mController.strengthregion.requestFocus();
        mFrame.repaint();
    }

    // interface for strength
    public void setStrength(String strength) {
        mLogger.message("Got " + strength);
        
        if (strength.equalsIgnoreCase("strength1")) {
            mController.checkSelectedStrength(mController.strength1);
            mController.strength1.setSelected(true);
        }
        if (strength.equalsIgnoreCase("strength2")) {
            mController.checkSelectedStrength(mController.strength2);
            mController.strength2.setSelected(true);
        }
        if (strength.equalsIgnoreCase("strength3")) {
            mController.checkSelectedStrength(mController.strength3);
            mController.strength3.setSelected(true);
        }
        if (strength.equalsIgnoreCase("strength4")) {
            mController.checkSelectedStrength(mController.strength4);
            mController.strength4.setSelected(true);
        }
        if (strength.equalsIgnoreCase("strength5")) {
            mController.checkSelectedStrength(mController.strength5);
            mController.strength5.setSelected(true);
        }
        if (strength.equalsIgnoreCase("strength6")) {
            mController.checkSelectedStrength(mController.strength6);
            mController.strength6.setSelected(true);
        }
        mFrame.repaint();
    }

    // interface for weakness
    public void setWeakness(String weakness) {
        mLogger.message("Got " + weakness);
        if (weakness.equalsIgnoreCase("weakness1")) {
            mController.checkSelectedWeakness(mController.weakness1);
            mController.weakness1.setSelected(true);
        }
        if (weakness.equalsIgnoreCase("weakness2")) {
            mController.checkSelectedWeakness(mController.weakness2);
            mController.weakness2.setSelected(true);
        }
        if (weakness.equalsIgnoreCase("weakness3")) {
            mController.checkSelectedWeakness(mController.weakness3);
            mController.weakness3.setSelected(true);
        }
        if (weakness.equalsIgnoreCase("weakness4")) {
            mController.checkSelectedWeakness(mController.weakness4);
            mController.weakness4.setSelected(true);
        }
        if (weakness.equalsIgnoreCase("weakness5")) {
            mController.checkSelectedWeakness(mController.weakness5);
            mController.weakness5.setSelected(true);
        }
        if (weakness.equalsIgnoreCase("weakness6")) {
            mController.checkSelectedWeakness(mController.weakness6);
            mController.weakness6.setSelected(true);
        }
        mFrame.repaint();
    }

    //interface for next
    public void next() {
        mLogger.message("Done!");
        mController.updateListeners();
    }
    
    private void initFX() {
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
        rootPane.setStyle("-fx-background-color: #FFFFFF00;");
        

        // build scene 
        Scene scene = new Scene(rootPane);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mJFXPanel.setScene(scene);

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
