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
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class UpdServer extends Thread {
    private DatagramSocket socket;
    private boolean running;
    private byte[] buf = new byte[16384];

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

                    synchronized (this) { // process the message at once, block other messages while processing
                        String message = new String(packet.getData(), 0, packet.getLength(), StandardCharsets.UTF_8);

                        mLogger.message("ODP UPD Message received: " + message);

                        JSONObject jObj = new JSONObject(message);

                        // check if object contains sentence element
                        if (jObj.has("sentence")) {
                            //JSONObject sentence = jObj.getJSONObject("sentence");
                            if (jObj.has("current_topics")) {
                                JSONObject topicsObj = jObj.getJSONObject("current_topics");
                                if (topicsObj.has("liwc-result")) {
                                    JSONArray topics = topicsObj.getJSONArray("liwc-result");

                                    Map<String, Float> topicValues = new HashMap<>();
                                    // iterate ...
                                    for (int tCnt = 0; tCnt < topics.length(); tCnt++) {
                                        JSONObject instance = topics.getJSONObject(tCnt);

                                        if (instance.has("I")) {
                                            topicValues.put("I", (float) instance.getDouble("I"));
                                        }
                                        if (instance.has("Self")) {
                                            topicValues.put("Self", (float) instance.getDouble("Self"));
                                        }
                                        if (instance.has("Job")) {
                                            topicValues.put("Job", (float) instance.getDouble("Job"));
                                        }
                                        if (instance.has("Friends")) {
                                            topicValues.put("Friends", (float) instance.getDouble("Friends"));
                                        }
                                    }

                                    if (topicValues.size() > 0) {
                                        // put highest ranked topic first
                                        Map<String, Float> sortedTopicValues = topicValues.entrySet()
                                                .stream()
                                                .sorted((Map.Entry.<String, Float>comparingByValue().reversed()))
                                                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (e1, e2) -> e1, LinkedHashMap::new));
                                        // TODO make VSM variable userUtteranceMainTopic configurable
                                        if (mProject.hasVariable("userUtteranceMainTopic")) {
                                            String firstTopic = sortedTopicValues.keySet().stream().findFirst().get();
                                            mProject.setVariable("userUtteranceMainTopic", new StringValue(firstTopic));
                                        }
                                    }
                                }
                            }
                        }


                        // check if object contains transcription element
                        if (jObj.has("transcription")) {
                            JSONObject transcript = jObj.getJSONObject("transcription");

                            String utterance = transcript.getString("utterance");
                            //float confidence = (float)transcript.getDouble("confidence");

                            // TODO make VSM variable userUtterance configurable
                            if (mProject.hasVariable("userUtterance")) {
                                mProject.setVariable("userUtterance", new StringValue(utterance));
                            }

                            // check if object contains source element
                            if (jObj.has("source")) { // "local" "asr_server"
                                String transSrcStr = jObj.getString("source");

                                if (mProject.hasVariable("transcriptionSource")) {
                                    mProject.setVariable("transcriptionSource", new StringValue(transSrcStr));
                                }
                            }

                            // check if object contains id element
                            if (jObj.has("id")) { // String UUID
                                String transIdStr = jObj.getString("id");

                                if (mProject.hasVariable("transcriptionID")) {
                                    mProject.setVariable("transcriptionID", new StringValue(transIdStr));
                                }
                            }

                            String allkeys = "";
                            //debug
                            for (Iterator iterator = jObj.keySet().iterator(); iterator.hasNext(); ) {
                                String key = (String) iterator.next();
                                allkeys = allkeys + " " + key.replace("\"", "");
                            }
                            if (mProject.hasVariable("debug")) {
                                mProject.setVariable("debug", new StringValue("Alle ODP messages keys " + allkeys));
                            }
                        }

                        if (jObj.has("liwc-result")) {
                            JSONObject liwcObj = jObj.getJSONObject("liwc-result");

                            // TODO make VSM variable userUtterance configurable
                            if (mProject.hasVariable("debug")) {
                                mProject.setVariable("debug", new StringValue("liwc daten " + liwcObj.toString().replace("\"", "").length()));
                            }

                            if (liwcObj.has("I. Basislinguistische Dimensionen")) {
                                JSONArray basisLingDim = liwcObj.getJSONArray("I. Basislinguistische Dimensionen");

                                if (basisLingDim.length() > 0) {
                                    JSONObject instance = basisLingDim.getJSONObject(0);

                                    if (instance.has("Pronomina (Gesamt)")) {
                                        JSONObject pronomina = instance.getJSONObject("Pronomina (Gesamt)");

                                        if (pronomina.has("Pronoun")) {
                                            float pronoun = (float) pronomina.getDouble("Pronoun");
                                            if (mProject.hasVariable("userUtterancePronoun")) {
                                                mProject.setVariable("userUtterancePronoun", new FloatValue(pronoun));
                                            }
                                        }

                                        if (pronomina.has("I")) {
                                            float myself = (float) pronomina.getDouble("I");
                                            if (mProject.hasVariable("userUtteranceMyself")) {
                                                mProject.setVariable("userUtteranceMyself", new FloatValue(myself));
                                            }
                                        }

                                        if (pronomina.has("We")) {
                                            float we = (float) pronomina.getDouble("We");
                                            if (mProject.hasVariable("userUtteranceWe")) {
                                                mProject.setVariable("userUtteranceWe", new FloatValue(we));
                                            }
                                        }

                                        if (pronomina.has("Self")) {
                                            float self = (float) pronomina.getDouble("Self");
                                            if (mProject.hasVariable("userUtteranceSelf")) {
                                                mProject.setVariable("userUtteranceSelf", new FloatValue(self));
                                            }
                                        }

                                        if (pronomina.has("You")) {
                                            float you = (float) pronomina.getDouble("You");
                                            if (mProject.hasVariable("userUtteranceYou")) {
                                                mProject.setVariable("userUtteranceYou", new FloatValue(you));
                                            }
                                        }

                                        if (pronomina.has("Other")) {
                                            float posAffect = (float) pronomina.getDouble("Other");
                                            if (mProject.hasVariable("userUtteranceOther")) {
                                                mProject.setVariable("userUtteranceOther", new FloatValue(posAffect));
                                            }
                                        }
                                    }
                                }
                            }

                            if (liwcObj.has("II. Psychologische Prozesse")) {
                                JSONArray psychCogProc = liwcObj.getJSONArray("II. Psychologische Prozesse");

                                if (psychCogProc.length() > 0) {
                                    JSONObject instance = psychCogProc.getJSONObject(0);

                                    if (instance.has("Affektive und emotionale Prozesse")) {
                                        JSONObject affProc = instance.getJSONObject("Affektive und emotionale Prozesse");

                                        if (affProc.has("Positiveemotion")) {
                                            float posAffect = (float) affProc.getDouble("Positiveemotion");
                                            if (mProject.hasVariable("userUtterancePosAffect")) {
                                                mProject.setVariable("userUtterancePosAffect", new FloatValue(posAffect));
                                            }
                                        }
                                        if (affProc.has("Negativeemotion")) {
                                            float negAffect = (float) affProc.getDouble("Negativeemotion");
                                            if (mProject.hasVariable("userUtteranceNegAffect")) {
                                                mProject.setVariable("userUtteranceNegAffect", new FloatValue(negAffect));
                                            }
                                        }
                                        if (affProc.has("Optimism")) {
                                            float optimism = (float) affProc.getDouble("Optimism");
                                            if (mProject.hasVariable("userUtteranceOptimism")) {
                                                mProject.setVariable("userUtteranceOptimism", new FloatValue(optimism));
                                            }
                                        }
                                        if (affProc.has("Anxiety")) {
                                            float anxiety = (float) affProc.getDouble("Anxiety");
                                            if (mProject.hasVariable("userUtteranceAnxiety")) {
                                                mProject.setVariable("userUtteranceAnxiety", new FloatValue(anxiety));
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
                                            float insight = (float) kogProc.getDouble("Insight");
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

                            if (liwcObj.has("IV. Persönliche Belange")) {
                                JSONArray psychCogProc = liwcObj.getJSONArray("IV. Persönliche Belange");

                                if (psychCogProc.length() > 0) {
                                    JSONObject instance = psychCogProc.getJSONObject(0);

                                    if (instance.has("Körperliche Zustände und Funktionen")) {
                                        JSONObject bodyStatesFcts = instance.getJSONObject("Körperliche Zustände und Funktionen");

                                        if (bodyStatesFcts.has("Body")) {
                                            float body = (float) bodyStatesFcts.getDouble("Body");
                                            if (mProject.hasVariable("userUtteranceBody")) {
                                                mProject.setVariable("userUtteranceBody", new FloatValue(body));
                                            }
                                        }
                                        if (bodyStatesFcts.has("Sex")) {
                                            float sex = (float) bodyStatesFcts.getDouble("Sex");
                                            if (mProject.hasVariable("userUtteranceSex")) {
                                                mProject.setVariable("userUtteranceSex", new FloatValue(sex));
                                            }
                                        }
                                        if (bodyStatesFcts.has("Eat")) {
                                            float eat = (float) bodyStatesFcts.getDouble("Eat");
                                            if (mProject.hasVariable("userUtteranceEat")) {
                                                mProject.setVariable("userUtteranceEat", new FloatValue(eat));
                                            }
                                        }
                                        if (bodyStatesFcts.has("Sleep")) {
                                            float sleep = (float) bodyStatesFcts.getDouble("Sleep");
                                            if (mProject.hasVariable("userUtteranceSleep")) {
                                                mProject.setVariable("userUtteranceSleep", new FloatValue(sleep));
                                            }
                                        }
                                    }

                                    if (instance.has("Berufs- und Ausbildungstätigkeit")) {
                                        JSONObject employment = instance.getJSONObject("Berufs- und Ausbildungstätigkeit");

                                        if (employment.has("School")) {
                                            float school = (float) employment.getDouble("School");
                                            if (mProject.hasVariable("userUtteranceSchool")) {
                                                mProject.setVariable("userUtteranceSchool", new FloatValue(school));
                                            }
                                        }
                                        if (employment.has("Job")) {
                                            float job = (float) employment.getDouble("Job");
                                            if (mProject.hasVariable("userUtteranceJob")) {
                                                mProject.setVariable("userUtteranceJob", new FloatValue(job));
                                            }
                                        }
                                        if (employment.has("Achieve")) {
                                            float achieve = (float) employment.getDouble("Achieve");
                                            if (mProject.hasVariable("userUtteranceAchieve")) {
                                                mProject.setVariable("userUtteranceAchieve", new FloatValue(achieve));
                                            }
                                        }
                                    }

                                    if (instance.has("Freizeit")) {
                                        JSONObject leisureTime = instance.getJSONObject("Freizeit");
                                    }

                                    if (instance.has("Metaphysische Themen")) {
                                        JSONObject meta = instance.getJSONObject("Metaphysische Themen");
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
