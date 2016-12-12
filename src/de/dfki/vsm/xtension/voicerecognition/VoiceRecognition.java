/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.voicerecognition;

import de.dfki.stickman3D.Stickman3D;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.stickman.StickmanExecutor;
import edu.cmu.sphinx.frontend.util.Microphone;
import edu.cmu.sphinx.recognizer.Recognizer;
import edu.cmu.sphinx.result.Result;
import edu.cmu.sphinx.util.props.ConfigurationManager;
import edu.cmu.sphinx.util.props.PropertyException;
import java.io.IOException;
import java.net.URL;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;

/**
 *
 * @author EmpaT
 */
public class VoiceRecognition extends Thread {

    RunTimeProject mProject;
    URL url;
    ConfigurationManager cm;
    Recognizer recognizer;
    Microphone microphone;

    boolean stopVoiceRecognition = false;

    public VoiceRecognition(RunTimeProject project) {
        this.mProject = project;
        try {
            System.out.println("Recording loaded...");
            this.url = getClass().getClassLoader().getResource("res/voicegrammer/voice.config.xml");
            this.cm = new ConfigurationManager(url);
            this.recognizer = (Recognizer) cm.lookup("recognizer");
            this.microphone = (Microphone) cm.lookup("microphone");
            recognizer.allocate();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void run() {
        if (microphone.startRecording()) {
            System.out.println("Recording started. Start speaking");
        }
        /////////////////////////
        while (!stopVoiceRecognition) {

            Result result = recognizer.recognize();

            if (result != null) {
                String resultText = result.getBestFinalResultNoFiller();
                System.out.println(resultText);
                String[] splitResultText = resultText.split(" ");
                String name = splitResultText[0];

                if (resultText.contains("engri")
                        || resultText.contains("angry")
                        || resultText.contains("aengri")
                        || resultText.contains("angri")
                        || resultText.contains("angry")
                        || resultText.contains("engry")) {
                    System.out.println("You said: " + name + " Angry/Angry please");
                    mProject.setVariable("action", name + " Angry");
                } else if (resultText.contains("joy")
                        || resultText.contains("joi")) {
                    System.out.println("You said: " + name + " Joy/Joy please");
                    mProject.setVariable("action", name + " Joy");
                } else if (resultText.contains("sad")
                        || resultText.contains("saad")
                        || resultText.contains("saed")
                        || resultText.contains("sed")
                        || resultText.contains("seed")) {
                    System.out.println("You said: " + name + " Sad/Sad please");
                    mProject.setVariable("action", name + " Sad");
                } else if (resultText.contains("fear")
                        || resultText.contains("feear")
                        || resultText.contains("fea")
                        || resultText.contains("feea")
                        || resultText.contains("feer")) {
                    System.out.println("You said: " + name + " Fear/Fear please");
                    mProject.setVariable("action", name + " Fear");
                } else if (resultText.contains("contempt")
                        || resultText.contains("cintempt")
                        || resultText.contains("kontempt")
                        || resultText.contains("kintempt")) {
                    System.out.println("You said: " + name + " Contempt/Contempt please");
                    mProject.setVariable("action", name + " Contempt");
                } else if (resultText.contains("disgusted")
                        || resultText.contains("disgasted")) {
                    System.out.println("You said: " + name + " Disgusted/Disgusted please");
                    mProject.setVariable("action", name + " Disgusted");
                } else if (resultText.contains("embarrassed")
                        || resultText.contains("embarrassd")
                        || resultText.contains("embaresd")
                        || resultText.contains("embaresed")
                        || resultText.contains("embauresd")
                        || resultText.contains("embaurest")
                        || resultText.contains("embaraset")
                        || resultText.contains("embaaresd")) {
                    System.out.println("You said: " + name + " Embarrassed/Embarrassed please");
                    mProject.setVariable("action", name + " Embarrassed");
                } else if (resultText.contains("excited")
                        || resultText.contains("excaited")
                        || resultText.contains("exited")
                        || resultText.contains("exaited")
                        || resultText.contains("eksaited")
                        || resultText.contains("eksited")
                        || resultText.contains("eqsaited")
                        || resultText.contains("eqsited")) {
                    System.out.println("You said: " + name + " Excited/Excited please");
                    mProject.setVariable("action", name + " Excited");
                } else if (resultText.contains("happy")
                        || resultText.contains("hapy")
                        || resultText.contains("hapi")
                        || resultText.contains("heppy")
                        || resultText.contains("hepy")
                        || resultText.contains("hepi")
                        || resultText.contains("happyy")
                        || resultText.contains("happii")
                        || resultText.contains("hapii")
                        || resultText.contains("heppyy")
                        || resultText.contains("heppii")
                        || resultText.contains("hafy")
                        || resultText.contains("haffy")
                        || resultText.contains("haffyy")
                        || resultText.contains("hafi")
                        || resultText.contains("faffii")
                        || resultText.contains("heffii")
                        || resultText.contains("hefii")) {
                    System.out.println("You said: " + name + " Happy/Happy please");
                    mProject.setVariable("action", name + " Happy");
                } else if (resultText.contains("Hello")
                        || resultText.contains("Hi")
                        || resultText.contains("hello")
                        || resultText.contains("hallo")
                        || resultText.contains("hellou")
                        || resultText.contains("hallou")
                        || resultText.contains("helou")
                        || resultText.contains("halou")
                        || resultText.contains("helo")
                        || resultText.contains("halo")) {
                    System.out.println("You said: " + name + " Hello/Hi");
                    mProject.setVariable("action", name + " Hello");
                } else if (resultText.contains("background")
                        || resultText.contains("beground")) {
                    if (resultText.contains("one")) {
                        switchBackground("bg1");
                    } else if (resultText.contains("two")) {
                        switchBackground("bg2");
                    } else if (resultText.contains("three")) {
                        switchBackground("bg3");
                    } else if (resultText.contains("four")) {
                        switchBackground("bg4");
                    } else if (resultText.contains("five")) {
                        switchBackground("bg5");
                    } else if (resultText.contains("zero")) {
                        switchBackground("zero");
                    }
                }
            }
        }
    }

    private void switchBackground(String background) {
        HBox stickmanBox = null;
        for (Map.Entry<String, Stickman3D> e : StickmanExecutor.stickmanContainer.entrySet()) {
            String stageID = e.getValue().getStickmanStageController().getStageIdentifier();
            try {
                stickmanBox = e.getValue().getStickmanStageController().getStickmanStage()
                        .getStickmanPane(stageID);
            } catch (Exception ex) {
                Logger.getLogger(VoiceRecognition.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        if (background.equalsIgnoreCase("zero")) {
            stickmanBox.setStyle("-fx-background-color: white");
        } else {
            String pathTobackground = getClass().getClassLoader().getResource("res/img/background/" + background + ".jpg").toExternalForm();
            stickmanBox.setStyle("-fx-background-image: url('" + pathTobackground + "'); "
                    + "-fx-background-position: center center; " + "-fx-background-repeat: stretch;");
        }
    }

}
