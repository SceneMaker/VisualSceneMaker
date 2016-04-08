/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.questionnaire;

import java.net.URL;
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
 * @author EmpaT
 */
public class FXMLDocumentController implements Initializable {

    @FXML
    private Button nextbutton;
    @FXML
    private TextField namefield;
    @FXML
    private Slider ageslider;
    @FXML
    private Slider jobinterviewslider;
    @FXML
    private CheckBox strength1;
    @FXML
    private CheckBox strength3;
    @FXML
    private CheckBox strength5;
    @FXML
    private CheckBox strength2;
    @FXML
    private CheckBox strength4;
    @FXML
    private CheckBox strength6;
    @FXML
    private CheckBox weakness1;
    @FXML
    private CheckBox weakness2;
    @FXML
    private CheckBox weakness3;
    @FXML
    private CheckBox weakness4;
    @FXML
    private CheckBox weakness5;
    @FXML
    private CheckBox weakness6;
    @FXML
    private RadioButton sexfemale;
    @FXML
    private RadioButton sexmale;
    @FXML
    private AnchorPane userinfo;
    @FXML
    private TextField agefield;
    @FXML
    private ToggleGroup sex;
    @FXML
    private TextField jobvinterviewfield;

    // internal variables
    private CheckBox lastSelectedStrength = null;
    private CheckBox sndlastSelectedStrength = null;
    private CheckBox thirdlastSelectedStrength = null;
    private CheckBox lastSelectedWeakness = null;
    private CheckBox sndlastSelectedWeakness = null;
    private CheckBox thirdlastSelectedWeakness = null;
    @FXML
    private Region strengthregion;
    @FXML
    private TextArea weaknessregion;

    /**
     * Initializes the controller class.
     */
    @Override

    public void initialize(URL url, ResourceBundle rb) {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                namefield.requestFocus();
            }
        });

    }

    @FXML
    private void nextbuttoncheck(MouseEvent event) {
        System.out.println("next");
       
    }

    @FXML
    private void namefieldcheck(KeyEvent event) {
        if (event.getCode() == KeyCode.ENTER) {
            System.out.println("namefieldvalue " + namefield.getText());
            ageslider.requestFocus();
            agefield.setText("20");
        }
    }

    @FXML
    private void ageslidercheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_DRAGGED) {
            int value = new Float(ageslider.getValue()).intValue();
            agefield.setText((20 + value) + "");
        }

        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            System.out.println("agefieldvalue " + agefield.getText());
            sexfemale.requestFocus();
        }

        if (event.getEventType() == MouseEvent.MOUSE_CLICKED) {
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
            System.out.println("jobvinterviewfieldvalue " + jobvinterviewfield.getText());
            strengthregion.requestFocus();
        }
        if (event.getEventType() == MouseEvent.MOUSE_CLICKED) {
            jobvinterviewfield.setText((jobvinterviewfield.getText().equalsIgnoreCase("") ? "keine" : jobvinterviewfield.getText()));
        }
    }

    @FXML
    private void strength1check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength1);
        }
    }

    @FXML
    private void strength3check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength3);
        }
    }

    @FXML
    private void strength5check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength5);
        }
    }

    @FXML
    private void strength2check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength2);
        }
    }

    @FXML
    private void strength4check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedStrength(strength4);
        }
    }

    @FXML
    private void strength6check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            System.out.println("strengths : none");
            strength1.setSelected(false);
            strength2.setSelected(false);
            strength3.setSelected(false);
            strength4.setSelected(false);
            strength5.setSelected(false);
            weaknessregion.requestFocus();
        }
    }

    @FXML
    private void weakness1check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness1);
        }
    }

    @FXML
    private void weakness2check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness2);
        }
    }

    @FXML
    private void weakness3check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness3);
        }
    }

    @FXML
    private void weakness4check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness4);
        }
    }

    @FXML
    private void weakness5check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            checkSelectedWeakness(weakness5);
        }
    }

    @FXML
    private void weakness6check(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            System.out.println("weakness : none");
            weakness1.setSelected(false);
            weakness2.setSelected(false);
            weakness3.setSelected(false);
            weakness4.setSelected(false);
            weakness5.setSelected(false);
            nextbutton.requestFocus();
        }
    }

    @FXML
    private void sexfemalecheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            System.out.println("sex " + ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            jobinterviewslider.requestFocus();
            jobvinterviewfield.setText("keine");
        }
    }

    @FXML
    private void sexmalecheck(MouseEvent event) {
        if (event.getEventType() == MouseEvent.MOUSE_RELEASED) {
            System.out.println("sex " + ((sex.getSelectedToggle() == sexfemale) ? "female" : "male"));
            jobinterviewslider.requestFocus();
            jobvinterviewfield.setText("keine");
        }
    }

    private void checkSelectedStrength(CheckBox strength) {
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

    private void checkSelectedWeakness(CheckBox weakness) {
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
