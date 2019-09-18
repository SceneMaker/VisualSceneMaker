/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.gui;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.activity.scheduler.ActivityWorker;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import javafx.application.Platform;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class GUIExecutor extends ActivityExecutor {

    // The GUI
    private static GUIRenderer mGUIRenderer = null;
    // The GUI thread
    Thread mGUIThread = null;
    // The current ActivityWorker
    ActivityWorker mActivityWorker = null;
    private final HashSet<ActivityWorker> mActivityWorkers = new HashSet<>();
    // Configuration values
    public final HashMap<String, GUIElementValues> mGUIElementIdValues = new HashMap<>();
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private static GUIExecutor sInstance;

    public GUIExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
        sInstance = this;
    }
    
    @Override
    public String marker(long id) {
        return "$(" + id + ")";
    }
    
    @Override
    public void execute(AbstractActivity activity) {
        if (activity instanceof SpeechActivity) {
            SpeechActivity sa = (SpeechActivity) activity;
            String text = sa.getTextOnly("$(").trim();
            LinkedList<String> timemarks = sa.getTimeMarks("$(");

            // If text is empty - assume activity has empty text but has marker activities registered
            if (text.isEmpty()) {
                for (String tm : timemarks) {
                    mLogger.warning("Directly executing activity at timemark " + tm);
                    mProject.getRunTimePlayer().getActivityScheduler().handle(tm);
                }
            }
        } else {
            final String name = activity.getName();

            if (name.equalsIgnoreCase("show")) {
                String[] elements = mProject.getAgentConfig(activity.getActor()).getProperty("show").split(",");
                mLogger.message("Showing gui elements ...");
                for (String e : elements) {
                    mLogger.message(e);
                }
                Platform.runLater(() -> mGUIRenderer.showGUIElements(elements));
            }
            
            if (name.equalsIgnoreCase("hide")) {
                Platform.runLater(() -> mGUIRenderer.hideGUIElements());
            }

            if (name.equalsIgnoreCase("set")) {

                String element = null;
                String value = null;

                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().contains("textfield")) {
                        element = af.getKey();
                        value = af.getVal();

                        if (mGUIElementIdValues.containsKey(element)) {
                            GUIElementValues gev = mGUIElementIdValues.get(element);
                            gev.mValue = value.replace("'", "");
                        }

                        mLogger.message("Assigning value of " + element + " with " + value);

                    }
                }
            }

            if (name.equalsIgnoreCase("get")) {

                String element = null;
                String value = null;

                for (ActionFeature af : activity.getFeatures()) {
                    if (af.getKey().contains("textfield")) {
                        element = af.getKey();
                        value = af.getVal();

                        if (mGUIElementIdValues.containsKey(element)) {
                            GUIElementValues gev = mGUIElementIdValues.get(element);

                            setVSmVar(gev.mVSMVar, gev.mValue);

                            mLogger.message("Storing value " + gev.mVSMVar + " of " + gev.mName + " in VSM var " + gev.mValue);
                        }


                    }
                }
            }
        }
    }
    
    public void setVSmVar(String var, String value) {
        mProject.setVariable(var, new StringValue(value));
    }

    public static GUIExecutor getInstance() {
        return sInstance;
    }


    protected String getProjectPath() {
        return mProject.getProjectPath();
    }

    protected String getProjectConfigVar(String k) {
        String value = null;

        for (ConfigFeature cf : mConfig.getEntryList()) {
            String key = cf.getKey();

            if (key.equalsIgnoreCase(k)) {
                value = cf.getValue();
                mLogger.message("Found CSS " + value);
            }
        }

        return value;
    }

    @Override
    public void launch() {
        mLogger.message("Launching GUIRenderer ...");
        // Since GUIRenderer is a JavaFX application it can only be executed once in the JVM!
        mGUIRenderer = (mGUIThread == null) ? new GUIRenderer() : mGUIRenderer;
        // give GUIRenderer the vsm executor instance
        mGUIRenderer.setButtonExecutor(this);
        mGUIThread = new Thread() {
            @Override
            public void run() {
                this.setName("GUIRenderer Thread");
                mGUIRenderer.create();
            }
        };
        mGUIThread.start();
        // format for button config
        // id, x, y, name, value, scenemaker var
        //<Feature key="button_yes" val="100, 100, <font size, e.g., 24>, "<name>", "yes_pressed", "<vsm var:String>"/>

        // format for image config
        // id, x, y, name, default value, scenemaker var
        //<Feature key="image_yes" val="100, 100, <font size, e.g., 24>, "<imagename>","<path to image>", "<vsm var:String>"/>

        // format for label config
        // id, x, y, name, value, scenemaker var
        //<Feature key="label_name" val="100, 100, <font size, e.g., 24>, "<name>", "<label name>", "<vsm var:String>"/>

        // format for rectangle config
        // id, x, y, name, default value, scenemaker var
        //<Feature key="rectangle_green" val="100, 100, <xsize>, <ysize>, "<rgb color + alpha, e.g., #FFFFFF00>", "<vsm var:String>"/>

        // format for text field config
        // id, x, y, name, default value, scenemaker var
        //<Feature key="textfield_yes" val="100, 100, <font size, e.g., 24>, "<Name>", "entername", "<vsm var:String>"/>


        //if (!mButtonGui.isInitialized()) {
//            String missingVariables = "";
//            int missingVarCnt = 0;
//            ArrayList<VarDef> globalVars = mProject.getSceneFlow().getParentNode().getVarDefList();

        for (ConfigFeature cf : mConfig.getEntryList()) {
            String key = cf.getKey();
            
            if (key.equalsIgnoreCase("hideonpressed")) {
                if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                    mGUIRenderer.mHideOnPressed = Boolean.parseBoolean(cf.getValue());
                }
            }
            if (key.equalsIgnoreCase("alwaysontop")) {
                if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                    mGUIRenderer.mAlwaysOnTop = Boolean.parseBoolean(cf.getValue());
                }
            }
            if (key.equalsIgnoreCase("takesallinput")) {
                if (cf.getValue().equalsIgnoreCase("true") || cf.getValue().equalsIgnoreCase("false")) {
                    mGUIRenderer.mModal = Boolean.parseBoolean(cf.getValue());
                }
            }

            if (key.contains("button")) {
                String[] values = cf.getValue().split(",");

                GUIElementValues bv = new GUIElementValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mLogger.message("Found button definition with id " + bv.mId + " @ " + bv.mX + "," + bv.mY + " (" + bv.mSize + "), name=" + bv.mName + ", value=" + bv.mValue + " vsmVar=" + bv.mVSMVar);

//                    // Check if scene_flow has the global variable values[5].trim();
//                    mLogger.message("Checking var " + values[5].trim());
//
//                    boolean hasVar = false;
//                    for (VarDef vd : globalVars) {
//                        hasVar = (!hasVar) ? vd.getName().equals(values[5].trim()) : false;
//                        if (hasVar) break;
//                    }
//                    
//                    if (!hasVar) {
//                        missingVarCnt++;
//                        missingVariables += values[5].trim() + ",";
//                    }
                mGUIElementIdValues.put(key, bv);
            }

            if (key.contains("image")) {
                String[] values = cf.getValue().split(",");

                GUIElementValues bv = new GUIElementValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mLogger.message("Found image definition with id " + bv.mId + " @ " + bv.mX + "," + bv.mY + " (" + bv.mSize + "), name=" + bv.mName + ", value=" + bv.mValue + " vsmVar=" + bv.mVSMVar);

                mGUIElementIdValues.put(key, bv);
            }

            if (key.contains("label")) {
                String[] values = cf.getValue().split(",");

                GUIElementValues bv = new GUIElementValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mLogger.message("Found label definition with id " + bv.mId + " @ " + bv.mX + "," + bv.mY + " (" + bv.mSize + "), name=" + bv.mName + ", value=" + bv.mValue + " vsmVar=" + bv.mVSMVar);

                mGUIElementIdValues.put(key, bv);
            }

            if (key.contains("rectangle")) {
                String[] values = cf.getValue().split(",");

                GUIElementValues bv = new GUIElementValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mLogger.message("Found rectangle definition with id " + bv.mId + " @ " + bv.mX + "," + bv.mY + " (" + bv.mSize + "x" + bv.mName + "), color=" + bv.mValue + " vsmVar=" + bv.mVSMVar);

                mGUIElementIdValues.put(key, bv);
            }

            if (key.contains("textfield")) {
                String[] values = cf.getValue().split(",");

                GUIElementValues bv = new GUIElementValues(key,
                        Integer.parseInt(values[0].trim()),
                        Integer.parseInt(values[1].trim()),
                        Integer.parseInt(values[2].trim()),
                        values[3].trim(),
                        values[4].trim(),
                        values[5].trim());

                mLogger.message("Found text field definition with id " + bv.mId + " @ " + bv.mX + "," + bv.mY + " (" + bv.mSize + "), name=" + bv.mName + ", value=" + bv.mValue + " vsmVar=" + bv.mVSMVar);

                mGUIElementIdValues.put(key, bv);
            }
        }
    }
    
    @Override
    public void unload() {
    }
}
