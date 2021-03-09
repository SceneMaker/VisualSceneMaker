package de.dfki.vsm.xtension.odp;

import de.dfki.vsm.runtime.interpreter.value.BooleanValue;
import de.dfki.vsm.runtime.interpreter.value.FloatValue;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;

public class UpdServer extends Thread {
    private DatagramSocket socket;
    private boolean running;
    private byte[] buf = new byte[2048];

    private RunTimeProject mProject;
    private final String mSceneFlowTaskVar = "odpTask";
    private final String mSceneFlowFuncVar = "odpFunc";
    private final String mSceneFlowContVar = "odpCont";
    private final String mSceneFlowNumbVar = "odpNum"; // stores the int value if odpCont is an int value
    private final String mSceneFlowActiVar = "odpAct";

    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();


    public UpdServer(int port, RunTimeProject project) throws SocketException {
        mProject = project;
        socket = new DatagramSocket(port);
    }

    @Override
    public void run() {
        running = true;
        mLogger.message("ODPExecutor: Ready to receive messages ...");
        while (running) {
            DatagramPacket packet = new DatagramPacket(buf, buf.length);
            try {
                if (packet.getLength() > 0) {
                    socket.receive(packet);
                }

                if (packet.getLength() > 0 && mProject.isRunning()) {
                    String message = new String(packet.getData(), 0, packet.getLength(), StandardCharsets.UTF_8);

                    mLogger.message("ODP UPD Message received: " + message);

                    JSONObject jObj = new JSONObject(message);


                    // check if object contains transcription element
                    if (jObj.has("transcription")) {
                        JSONObject transcript = jObj.getJSONObject("transcription");

                        String utterance = transcript.getString("utterance");
                        //float confidence = (float)transcript.getDouble("confidence");

                        // TODO make VSM variable userUtterance configurable
                        if (mProject.hasVariable("userUtterance")) {
                            mProject.setVariable("userUtterance", new StringValue(utterance));
                        }

                        String allkeys = "";
                        //debug
                        for (Iterator iterator = jObj.keySet().iterator(); iterator.hasNext(); ) {
                            String key = (String) iterator.next();
                            allkeys = allkeys + " " + key.replace("\"", "");
                        }
                        mProject.setVariable("debug", new StringValue("Alle ODP messages keys " + allkeys));
                    }

                    if (jObj.has("liwc-result")) {
                        JSONObject liwcObj = jObj.getJSONObject("liwc-result");

                        // TODO make VSM variable userUtterance configurable
                        if (mProject.hasVariable("debug")) {
                            mProject.setVariable("debug", new StringValue("liwc daten " + liwcObj.toString().replace("\"", "").length()));
                        }

                        if (liwcObj.has("II. Psychologische Prozesse")) {
                            JSONArray psychCogProc = liwcObj.getJSONArray("II. Psychologische Prozesse");

                            if (psychCogProc.length() > 0) {
                                JSONObject instance = psychCogProc.getJSONObject(0);

                                if (instance.has("Affektive und emotionale Prozesse")) {
                                    JSONObject affProc = instance.getJSONObject("Affektive und emotionale Prozesse");

                                    if (affProc.has("Positiveemotion")) {
                                        int posAffect = affProc.getInt("Positiveemotion");
                                        if (mProject.hasVariable("userUtterancePosAffect")) {
                                            mProject.setVariable("userUtterancePosAffect", new IntValue(posAffect));
                                        }
                                    }
                                }

                                if (instance.has("Kognitive Prozesse")) {
                                    JSONObject kogProc = instance.getJSONObject("Kognitive Prozesse");

                                    if (kogProc.has("Discrepancy")) {
                                        float discrepancy = (float) kogProc.getDouble("Discrepancy");
                                        if (mProject.hasVariable("userUtteranceDiscrepancy")) {
                                            mProject.setVariable("userUtteranceDiscrepancy", new FloatValue(discrepancy));
                                        }
                                    }

                                    if (kogProc.has("Insight")) {
                                        float insight = kogProc.getDouble("Insight");
                                        if (mProject.hasVariable("userUtteranceInsight")) {
                                            mProject.setVariable("userUtteranceInsight", new FloatValue(insight));
                                        }
                                    }

                                    if (kogProc.has("Tentative")) {
                                        float tenative = (float) kogProc.getDouble("Tentative");
                                        if (mProject.hasVariable("userUtteranceTentative")) {
                                            mProject.setVariable("userUtteranceTentative", new FloatValue(tenative));
                                        }
                                    }
                                }
                            }
                        }

                        if (liwcObj.has("Gesamtwortzahl")) {
                            int totalWords = liwcObj.getInt("Gesamtwortzahl");

                            // TODO make VSM variable userUtteranceTotalWords configurable
                            if (mProject.hasVariable("userUtteranceTotalWords")) {
                                mProject.setVariable("userUtteranceTotalWords", new IntValue(totalWords));
                            }
                        }
                    }

                    if (jObj.has("inferred-topics")) {
                        String topics = jObj.getString("inferred-topics");
                        topics = topics.replace(" ", ""); // remove spaces
                        topics = (topics.startsWith(",")) ? topics.substring(1) : topics;

                        // TODO make VSM variable userUtteranceTotalWords configurable
                        if (mProject.hasVariable("userUtteranceTopics")) {
                            mProject.setVariable("userUtteranceTopics", new StringValue(topics));
                        }
                    }

                    if (jObj.has("odpstate")) {
                        String value = jObj.getString("odpstate");

                        // TODO make VSM variable odpstate configurable
                        if (mProject.hasVariable("odpstate")) {
                            mProject.setVariable("odpstate", new StringValue(value));
                        }

                        if (value.equalsIgnoreCase("startSpeech")) {
                            // TODO make VSM variable user_speaking configurable
                            if (mProject.hasVariable("user_speaking")) {
                                mProject.setVariable("user_speaking", new BooleanValue(true));
                            }
                        }

                        if (value.equalsIgnoreCase("stopSpeech")) {
                            // TODO make VSM variable user_speaking configurable
                            if (mProject.hasVariable("user_speaking")) {
                                mProject.setVariable("user_speaking", new BooleanValue(false));
                            }
                        }
                    }

                    // task
                    String task = "";
                    try {
                        task = jObj.getString("task");
                    } catch (Exception e) {
                        task = "";
                    }

                    // function
                    String function = "";
                    try {
                        function = jObj.getString("function");
                    } catch (Exception e) {
                        function = "";
                    }

                    // content
                    String content = "";
                    try {
                        content = jObj.getString("content");
                    } catch (Exception e) {
                        content = "";
                    }

                    // int content
                    int number = -1;
                    try {
                        number = Integer.parseInt(content);
                    } catch (NumberFormatException e) {
                        number = -1;
                    }

                    // action
                    String action = "";
                    try {
                        action = jObj.getString("action");
                    } catch (Exception e) {
                        action = "";
                    }

                    // ContactName
                    String ContactName = "";
                    try {
                        ContactName = jObj.getString("ContactName");
                    } catch (Exception e) {
                        ContactName = "";
                    }

                    // ContactNumber
                    String ContactNumber = "";
                    try {
                        ContactNumber = jObj.getString("ContactNumber");
                    } catch (Exception e) {
                        ContactNumber = "";
                    }

                    mProject.setVariable(mSceneFlowTaskVar, new StringValue(task));
                    mProject.setVariable(mSceneFlowFuncVar, new StringValue(function));
                    mProject.setVariable(mSceneFlowContVar, new StringValue(content));
                    mProject.setVariable(mSceneFlowNumbVar, number);
                    mProject.setVariable(mSceneFlowActiVar, new StringValue(action));

                    mLogger.message("Parsed ODP Message: " + jObj);
                }
            }
            catch (IOException e) {
                mLogger.failure(e.toString());
            }
        }
    }

    public void close() {
        running = false;
        socket.close();
    }
}
