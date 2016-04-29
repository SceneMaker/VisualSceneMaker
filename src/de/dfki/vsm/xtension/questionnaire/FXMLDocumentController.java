/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.ResourceBundle;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.Slider;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Region;

/**
 * FXML Controller class
 *
 * @author Patrick Gebhard
 */
public class FXMLDocumentController implements Initializable {

    @FXML
    protected Button nextbutton;
    @FXML
    protected TextField namefield;
    @FXML
    protected Slider ageslider;
    @FXML
    protected Slider jobinterviewslider;
    @FXML
    protected CheckBox strength1;
    @FXML
    protected CheckBox strength3;
    @FXML
    protected CheckBox strength5;
    @FXML
    protected CheckBox strength2;
    @FXML
    protected CheckBox strength4;
    @FXML
    protected CheckBox strength6;
    @FXML
    protected CheckBox weakness1;
    @FXML
    protected CheckBox weakness2;
    @FXML
    protected CheckBox weakness3;
    @FXML
    protected CheckBox weakness4;
    @FXML
    protected CheckBox weakness5;
    @FXML
    protected CheckBox weakness6;
    @FXML
    protected RadioButton sexfemale;
    @FXML
    protected RadioButton sexmale;
    @FXML
    private AnchorPane userinfo;
    @FXML
    protected TextField agefield;
    @FXML
    protected ToggleGroup sex;
    @FXML
    protected TextField jobvinterviewfield;

    // internal variables
    private CheckBox lastSelectedStrength = null;
    private CheckBox sndlastSelectedStrength = null;
    private CheckBox thirdlastSelectedStrength = null;
    private CheckBox lastSelectedWeakness = null;
    private CheckBox sndlastSelectedWeakness = null;
    private CheckBox thirdlastSelectedWeakness = null;
    // listeners for updates
    private ArrayList<QuestionnaireListener> mListeners = new java.util.ArrayList();
    // collected values
    private HashMap<String, String> mValues = new HashMap();

    @FXML
    protected Region strengthregion;
    @FXML
    protected TextArea weaknessregion;

    public void addListener(QuestionnaireListener listener) {
        if (!mListeners.contains(listener)) {
            mListeners.add(listener);
        }
    }

    /**
     * Initializes the controller class.
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        mValues.put("name", "Maier");
        mValues.put("age", "20");
        mValues.put("sex", "female");
        mValues.put("interviews", "keine");
        mValues.put("strength1", "nein");
        mValues.put("strength2", "nein");
        mValues.put("strength3", "nein");
        mValues.put("strength4", "nein");
        mValues.put("strength5", "nein");
        mValues.put("strength6", "nein");
        mValues.put("weakness1", "nein");
        mValues.put("weakness2", "nein");
        mValues.put("weakness3", "nein");
        mValues.put("weakness4", "nein");
        mValues.put("weakness5", "nein");
        mValues.put("weakness6", "nein");

        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                
                nextbutton.setDisable(true);
            }
        });

    }
    
    protected void updateListeners() {
        for (QuestionnaireListener l : mListeners) {
            l.updateOnUestionnaire(mValues);
        }
    }

    @FXML
    private void nextbuttoncheck(MouseEvent event) {
        mValues.put("name", namefield.getText());
        
        updateListeners();
    }

    @FXML
    private void namefieldcheck(KeyEvent event) {
        if (event.getCode() == KeyCode.ENTER) {
            //System.out.println("namefieldvalue " + namefield.getText());
            mValues.put("name", namefield.getText());
            ageslider.requestFocus();
            agefield.setText("20");
        }
        nextbutton.setDisable(false);
    }

    @FXML
    private void ageslidercheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_DRAGGED) {
            int value = new Float(ageslider.getValue()).intValue();
            agefield.setText((20 + value) + "");
        }

        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("agefieldvalue " + agefield.getText());
            mValues.put("age", agefield.getText());
            sexfemale.requestFocus();
        }

        if (event.getEventType() == MouseEvent.MOUSE_CLICKED) {
            nextbutton.setDisable(false);
            agefield.setText((agefield.getText().equalsIgnoreCase("") ? "20" : agefield.getText()));
        }
    }

    @FXML
    private void jobinterviewslidercheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_DRAGGED) {
            int value = new Float(jobinterviewslider.getValue()).intValue();
            if (value == 0) {
                jobvinterviewfield.setText("keine");
            }
            if (value > 0 && value < 9) {
                jobvinterviewfield.setText(value + "");
            }
            if (value > 9) {
                jobvinterviewfield.setText("mehr als 8");
            }
        }

        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("jobvinterviewfieldvalue " + jobvinterviewfield.getText());
            mValues.put("interviews", jobvinterviewfield.getText());
            strengthregion.requestFocus();
        }
        if (event.getEventType() == MouseEvent.MOUSE_CLICKED) {
            jobvinterviewfield.setText((jobvinterviewfield.getText().equalsIgnoreCase("") ? "keine" : jobvinterviewfield.getText()));
            mValues.put("interviews", jobvinterviewfield.getText());
        }
    }

    @FXML
    private void strength1check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength1);
            mValues.put("strength1", (strength1.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void strength3check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength3);
            mValues.put("strength3", (strength3.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void strength5check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength5);
            mValues.put("strength5", (strength5.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void strength2check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength2);
            mValues.put("strength2", (strength2.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void strength4check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength4);
            mValues.put("strength4", (strength4.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void strength6check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("strengths : none");
            mValues.put("strength6", (strength6.isSelected() ? "ja" : "nein"));
            strength1.setSelected(false);
            mValues.put("strength1", (strength1.isSelected() ? "ja" : "nein"));
            strength2.setSelected(false);
            mValues.put("strength2", (strength2.isSelected() ? "ja" : "nein"));
            strength3.setSelected(false);
            mValues.put("strength3", (strength3.isSelected() ? "ja" : "nein"));
            strength4.setSelected(false);
            mValues.put("strength4", (strength4.isSelected() ? "ja" : "nein"));
            strength5.setSelected(false);
            mValues.put("strength5", (strength5.isSelected() ? "ja" : "nein"));
            weaknessregion.requestFocus();
        }
    }

    @FXML
    private void weakness1check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness1);
            mValues.put("weakness1", (weakness1.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void weakness2check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness2);
            mValues.put("weakness2", (weakness2.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void weakness3check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness3);
            mValues.put("weakness3", (weakness3.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void weakness4check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness4);
            mValues.put("weakness4", (weakness4.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void weakness5check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness5);
            mValues.put("weakness5", (weakness5.isSelected() ? "ja" : "nein"));
        }
    }

    @FXML
    private void weakness6check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("weakness : none");
            mValues.put("weakness6", (weakness6.isSelected() ? "ja" : "nein"));
            weakness1.setSelected(false);
            mValues.put("weakness1", (weakness1.isSelected() ? "ja" : "nein"));
            weakness2.setSelected(false);
            mValues.put("weakness2", (weakness2.isSelected() ? "ja" : "nein"));
            weakness3.setSelected(false);
            mValues.put("weakness3", (weakness3.isSelected() ? "ja" : "nein"));
            weakness4.setSelected(false);
            mValues.put("weakness4", (weakness4.isSelected() ? "ja" : "nein"));
            weakness5.setSelected(false);
            mValues.put("weakness5", (weakness5.isSelected() ? "ja" : "nein"));
            nextbutton.requestFocus();
        }
    }

    @FXML
    private void sexfemalecheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("sex " + ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            mValues.put("sex", ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            jobinterviewslider.requestFocus();
            jobvinterviewfield.setText("keine");
        }
    }

    @FXML
    private void sexmalecheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            //System.out.println("sex " + ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            mValues.put("sex", ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            jobinterviewslider.requestFocus();
            jobvinterviewfield.setText("keine");
        }
    }

    protected void checkSelectedStrength(CheckBox strength) {
        strength6.setSelected(false);

        if (strength.isSelected()) {
            thirdlastSelectedStrength = sndlastSelectedStrength;
            sndlastSelectedStrength = lastSelectedStrength;
            lastSelectedStrength = strength;
        }

        int numStrengthSelected = 0;
        numStrengthSelected
                = (strength1.isSelected() ? 1 : 0)
                + (strength2.isSelected() ? 1 : 0)
                + (strength3.isSelected() ? 1 : 0)
                + (strength4.isSelected() ? 1 : 0)
                + (strength5.isSelected() ? 1 : 0);

        if (numStrengthSelected == 2) {
            weaknessregion.requestFocus();
        }

        if (numStrengthSelected == 3) {
            thirdlastSelectedStrength.setSelected(false);
        }
    }

    protected void checkSelectedWeakness(CheckBox weakness) {
        weakness6.setSelected(false);

        if (weakness.isSelected()) {
            thirdlastSelectedWeakness = sndlastSelectedWeakness;
            sndlastSelectedWeakness = lastSelectedWeakness;
            lastSelectedWeakness = weakness;
        }

        int numWeaknessSelected = 0;
        numWeaknessSelected
                = (weakness1.isSelected() ? 1 : 0)
                + (weakness2.isSelected() ? 1 : 0)
                + (weakness3.isSelected() ? 1 : 0)
                + (weakness4.isSelected() ? 1 : 0)
                + (weakness5.isSelected() ? 1 : 0);

        if (numWeaknessSelected == 2) {
            nextbutton.requestFocus();
        }

        if (numWeaknessSelected == 3) {
            thirdlastSelectedWeakness.setSelected(false);
        }
    }
}
