package de.dfki.vsm.xtension.tricatworld;

import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.ListValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventEntry;
import de.dfki.vsm.xtension.ssi.event.data.SSIEventData;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.ssi.event.data.SSITupleData;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class TriCatWorldSSIPlugin extends SSIRunTimePlugin {

    // The map of processes
    private final HashMap<String, Process> mProcessMap
            = new HashMap();
    // The flag if we use the JPL
    private final boolean mUseJPL;
    // The flag for executables
    private final boolean mUseExe;
    // The flag for keyword list
    private final boolean mUseLst;
    // The flag if we use SSI superevents 
    private final boolean mUseSuperEvent;

    // Construct SSI plugin
    public TriCatWorldSSIPlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        super(config, project);
        // Get the list flag value
        mUseLst = Boolean.parseBoolean(
                mConfig.getProperty("uselst"));
        // Get the JPL flag value
        mUseJPL = Boolean.parseBoolean(
                mConfig.getProperty("usejpl"));
        // Get the executable flag value
        mUseExe = Boolean.parseBoolean(
                mConfig.getProperty("useexe"));
        // Get the super event value
        mUseSuperEvent = Boolean.parseBoolean(
                mConfig.getProperty("usesuperevent"));
    }

    // Launch SSI plugin
    @Override
    public void launch() {
        super.launch();

        // Load the plugin configuration
        final String ssidir = mConfig.getProperty("ssidir");
        final String ssibat = mConfig.getProperty("ssibat");

        // Added PG - 10.1.2016 for our convenience
        if (mUseExe) {
            // Create the plugin's processes
            try {
                mProcessMap.put(ssibat, Runtime.getRuntime().exec(
                        "cmd /c start " + ssibat + "" + "", null, new File(ssidir)));
            } catch (final IOException exc) {
                mLogger.failure(exc.toString());
            }
            // Print some information 
            mLogger.message("Launching TWorld SSI Plugin");
        }
    }

    // Unload SSI plugin
    @Override
    public void unload() {
        super.unload();
        // Wait for pawned processes
        for (final Entry<String, Process> entry : mProcessMap.entrySet()) {
            // Get the process entry
            final String name = entry.getKey();
            final Process process = entry.getValue();
            try {
                // Kill the processes
                if (mUseExe) {
                    final Process killer = Runtime.getRuntime().exec("taskkill /F /FI \"IMAGENAME eq xmlpipe.exe\"");
                    // Wait for the killer
                    killer.waitFor();
                    // Print some information 
                    mLogger.message("Joining killer " + name + "");
                }
                // Wait for the process
                process.waitFor();
                // Print some information 
                mLogger.message("Joining process " + name + "");
            } catch (final IOException | InterruptedException exc) {
                mLogger.failure(exc.toString());
            }
        }
        // Print some information 
        mLogger.message("Unloading TWorld SSI Plugin");
    }

    // Handle SSI event array
    @Override
    public void handle(final SSIEventArray array) {
        // Print some information 
        for (final SSIEventEntry event : array.list()) {
            final SSIEventData data = event.getData();
            if (!mUseSuperEvent) {
                if (event.getSender().equalsIgnoreCase("fubi")) {
                    final String name = event.getEvent().toLowerCase();
                    String variable = null;
                    switch (name) {
                        case "armsopen":
                            variable = "UserArmsAreOpened";
                            break;
                        case "armscrossed":
                            variable = "UserArmsAreCrossed";
                            break;
                        case "righthandheadtouch":
                            variable = "UserRightHandAtHead";
                            break;
                        case "lefthandheadtouch":
                            variable = "UserLeftHandAtHead";
                            break;
                        case "lookleft":
                            variable = "UserIsLookingLeft";
                            break;
                        case "lookright":
                            variable = "UserIsLookingRight";
                            break;
                        case "waving":
                            variable = "UserIsWaving";
                            break;
                        default:
                            // nothing
                            break;
                    }
                    if (variable != null) {
                        if (event.getState().equalsIgnoreCase("completed")) {
                            //mLogger.message("User stopped " + variable);
                            if (mUseJPL) {
                                // TODO 
                            } else {
                                if (variable.equalsIgnoreCase("UserIsWaving")) {
                                    mProject.setVariable(variable, true);
                                } else {
                                    mProject.setVariable(variable, false);
                                }
                            }
                        } else if (event.getState().equalsIgnoreCase("continued")) {
                            //mLogger.message("User started " + variable);
                            if (mUseJPL) {
                                // TODO 
                            } else {
                                // Set variable
                                mProject.setVariable(variable, true);
                            }
                        } else {
                            //
                        }
                    }
                } else if (event.getSender().equalsIgnoreCase("nova")
                        && event.getEvent().equalsIgnoreCase("body_properties_MNV")) { // added by PG 14.6.2017 (based on the nova event pipe)

                    SSITupleData tupleData = (SSITupleData) data;
            
                    mProject.setVariable("UserHeadOrientation", Float.parseFloat(tupleData.get("headorientation")));
                    mProject.setVariable("UserHeadTilt", Float.parseFloat(tupleData.get("headtilt")));
                    mProject.setVariable("UserHeadNod", Float.parseFloat(tupleData.get("headnod")));
                    mProject.setVariable("UserLeanPosture", Float.parseFloat(tupleData.get("leanposture")));

                } else if (event.getSender().equalsIgnoreCase("shore")
                        && event.getEvent().equalsIgnoreCase("head")) {
                    final String content = ((SSIStringData) data).toString();
                    final Document element = XMLUtilities.xmlStringToDocument(content);
                    final NodeList childs = element.getElementsByTagName("head");
                    for (int i = 0; i < childs.getLength(); i++) {
                        // Get the nex tuple element
                        final Element head = ((Element) childs.item(i));
                        try {
                            mProject.setVariable("UserHeadPositionX",
                                    Float.parseFloat(head.getAttribute("x")));
                            mProject.setVariable("UserHeadPositionY",
                                    Float.parseFloat(head.getAttribute("y")));
                        } catch (final NumberFormatException exc) {
                            // Do nothing
                        }
                    }
                } else if (event.getSender().equalsIgnoreCase("face")
                        && event.getEvent().equalsIgnoreCase("smile")) {
                    if (event.getState().equalsIgnoreCase("completed")) {
                        //mLogger.message("User stopped smiling (Kinect)");
                        if (mUseJPL) {
                            // TODO 
                        } else {
                            // Set speaking variable
                            mProject.setVariable("UserSmiling", false);
                        }
                    } else if (event.getState().equalsIgnoreCase("continued")) {
                        //mLogger.message("User started smiling (Kinect)");

                        if (mUseJPL) {
                            // TODO 
                        } else {
                            // Set speaking variable
                            mProject.setVariable("UserSmiling", true);
                        }
                    }
                } else if (event.getSender().equalsIgnoreCase("audio")
                        && event.getEvent().equalsIgnoreCase("vad")) {
                    if (event.getState().equalsIgnoreCase("completed")) {
                        // User stopped speaking
                        //mLogger.message("User stopped speaking");
                        if (mUseJPL) {
                            JPLEngine.query("now(Time), "
                                    + "jdd(["
                                    + "type:" + "event" + ","
                                    + "mode:" + "voice" + ","
                                    + "name:" + "user" + ","
                                    + "time:" + "Time" + ","
                                    + "from:" + event.getFrom() + ","
                                    + "life:" + event.getDur() + ","
                                    + "conf:" + event.getProb() + ","
                                    + "data:" + "stop"
                                    + "]).");
                        } else {
                            // Set speaking variable
                            mProject.setVariable("UserIsSpeaking", false);
                        }
                    } else if (event.getState().equalsIgnoreCase("continued")) {
                        // User started speaking
                        //mLogger.message("User started speaking");
                        if (mUseJPL) {
                            JPLEngine.query("now(Time), "
                                    + "jdd(["
                                    + "type:" + "event" + ","
                                    + "mode:" + "voice" + ","
                                    + "name:" + "user" + ","
                                    + "time:" + "Time" + ","
                                    + "from:" + event.getFrom() + ","
                                    + "life:" + event.getDur() + ","
                                    + "conf:" + event.getProb() + ","
                                    + "data:" + "start"
                                    + "]).");
                        } else {
                            // Set speaking variable
                            mProject.setVariable("UserIsSpeaking", true);
                        }
                    } else {
                        // Should not happen
                    }
                } else if (event.getSender().equalsIgnoreCase("speech")
                        && event.getEvent().equalsIgnoreCase("act")) {
                    final String keyword = ((SSIStringData) data).toString().trim();
                    // User started speaking
                    mLogger.message("User said '" + keyword + "'");
                    if (mUseJPL) {
                        JPLEngine.query("now(Time), "
                                + "jdd(["
                                + "type:" + "event" + ","
                                + "mode:" + "speech" + ","
                                + "name:" + "user" + ","
                                + "time:" + "Time" + ","
                                + "from:" + event.getFrom() + ","
                                + "life:" + event.getDur() + ","
                                + "conf:" + event.getProb() + ","
                                + "data:" + "'" + keyword + "'"
                                + "]).");
                    } else {
                        // Set keyword variable
                        mProject.setVariable("UserSaidKeyword", keyword);

                        if (mUseLst) {
                            final AbstractValue value = mProject.getValueOf("KeywordList");
                            if (value instanceof ListValue) {
                                final ListValue listValue = (ListValue) value;
                                final LinkedList list = listValue.getValueList();
                                if (list != null) {
                                    list.add(new StringValue(keyword));
                                    mProject.setVariable("KeywordList", listValue);
                                } else {
                                    mLogger.failure("No internal list available");
                                }
                            } else {
                                mLogger.failure("No keyword list variable");
                            }

                        }

                    }
                    // This condition is used to receive structure sent by SSI
                } else {
                    // This should not happen
                }
            } else if (data instanceof SSIStringData) {
                final TriCatWorldSSIData mSSIData = new TriCatWorldSSIData(
                        ((SSIStringData) array.list().get(0).getData()).toString());

                //mLogger.message("Handling SSI data " + mSSIData);
                final HashMap<String, AbstractValue> values = new HashMap<>();
                values.put("voice_activity", new StringValue(mSSIData.get("voice.activity")));
                String detectedUtterance = mSSIData.get("voice.speechact");
                values.put("voice_keyword", new StringValue(detectedUtterance));

                if (!detectedUtterance.isEmpty()) {
                    // PG - added 3.5.2016, check if uttrance is nummeric
                    try {
                        Integer.parseInt(detectedUtterance);
                        values.put("voice_act_is_nummeric", new StringValue("1"));
                    } catch (NumberFormatException nfe) {
                        values.put("voice_act_is_nummeric", new StringValue("0"));
                    }
                } else {
                    values.put("voice_act_is_nummeric", new StringValue(""));
                }

                values.put("voice_praat_pitchmean", new StringValue(mSSIData.get("voice.praat.pitchmean")));
                values.put("voice_praat_pitchsd", new StringValue(mSSIData.get("voice.praat.pitchsd")));
                values.put("voice_praat_speechrate", new StringValue(mSSIData.get("voice.praat.speechrate")));
                values.put("voice_praat_intensity", new StringValue(mSSIData.get("voice.praat.intensity")));
                values.put("head_position_x", new StringValue(mSSIData.get("head.position.x")));
                values.put("head_position_y", new StringValue(mSSIData.get("head.position.y")));
                values.put("head_orientation_roll", new StringValue(mSSIData.get("head.orientation.roll")));
                float a = Float.parseFloat(mSSIData.get("head.orientation.pitch")) * -1.0f;
                values.put("head_orientation_pitch", new FloatValue(a));
                values.put("head_orientation_yaw", new StringValue(mSSIData.get("head.orientation.yaw")));
                values.put("head_movement_nod", new StringValue(mSSIData.get("head.movement.nod")));
                values.put("head_movement_shake", new StringValue(mSSIData.get("head.movement.shake")));
                float ba = Float.parseFloat(mSSIData.get("body.activity"));
                values.put("body_activity", new FloatValue(ba));
                values.put("body_energy", new StringValue(mSSIData.get("body.energy")));
                //                values.put("body_posture_leanfront_detected", new StringValue(mSSIData.get("body.posture.leanfront.detected")));
                //                values.put("body_posture_leanfront_duration", new StringValue(mSSIData.get("body.posture.leanfront.duration")));
                //                values.put("body_posture_leanfront_intensity", new StringValue(mSSIData.get("body.posture.leanfront.intensity")));
                //                values.put("body_posture_leanback_detected", new StringValue(mSSIData.get("body.posture.leanback.detected")));
                //                values.put("body_posture_leanback_duration", new StringValue(mSSIData.get("body.posture.leanback.duration")));
                //                values.put("body_posture_leanback_intensity", new StringValue(mSSIData.get("body.posture.leanback.intensity")));
                //                values.put("body_gesture_armsopen_detected", new StringValue(mSSIData.get("body.gesture.armsopen.detected")));
                //                values.put("body_gesture_armsopen_duration", new StringValue(mSSIData.get("body.gesture.armsopen.duration")));
                //                values.put("body_gesture_armsopen_intensity", new StringValue(mSSIData.get("body.gesture.armsopen.intensity")));
                //                values.put("body_gesture_armscrossed_detected", new StringValue(mSSIData.get("body.gesture.armscrossed.detected")));
                //                values.put("body_gesture_armscrossed_duration", new StringValue(mSSIData.get("body.gesture.armscrossed.duration")));
                //                values.put("body_gesture_armscrossed_intensity", new StringValue(mSSIData.get("body.gesture.armscrossed.intensity")));
                //                values.put("body_gesture_lefthandheadtouch_detected", new StringValue(mSSIData.get("body.gesture.lefthandheadtouch.detected")));
                //                values.put("body_gesture_lefthandheadtouch_duration", new StringValue(mSSIData.get("body.gesture.lefthandheadtouch.duration")));
                //                values.put("body_gesture_righthandheadtouch_detected", new StringValue(mSSIData.get("body.gesture.righthandheadtouch.detected")));
                //                values.put("body_gesture_righthandheadtouch_duration", new StringValue(mSSIData.get("body.gesture.righthandheadtouch.duration")));
                values.put("face_expression_smile_detected", new StringValue(mSSIData.get("face.expression.smile.detected")));
                values.put("face_expression_smile_duration", new StringValue(mSSIData.get("face.expression.smile.duration")));
                values.put("face_expression_smile_intensity", new StringValue(mSSIData.get("face.expression.smile.intensity")));

                //mProject.setVariable("usercues", new StructValue(values));
                for (final Entry<String, AbstractValue> value : values.entrySet()) {
                    if (mProject.hasVariable(value.getKey())) {
                        mProject.setVariable(value.getKey(), value.getValue());
                        //mLogger.message("Setting Variable " + value.getKey() + " to " + value.getValue().getConcreteSyntax());
                    } else {
                        //mLogger.warning("Variable " + value.getKey() + " not defined!");
                    }
                }
            }
        }
    }
}
