package de.dfki.vsm.xtension.baxter.utils.messagehandlers.VAD;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.baxter.BaxterExecutor;
import de.dfki.vsm.xtension.baxter.utils.messagehandlers.BaxterMessageHandler;

/**
 * Created by alvaro on 22.09.16.
 */
public class VAD {
    private static VAD sInstane = null;
    static WaitForSpeech waitForSpeech;
    static BaxterMessageHandler baxterMessageHandler;
    static RunTimeProject project;
    static BaxterExecutor baxterExecutor;
    private VAD(){
    }

    public static void setBaxterMessageHandler(BaxterMessageHandler messageHandler){
        baxterMessageHandler = messageHandler;
    }

    public static void setProject(RunTimeProject p){
        project = p;
    }

    public static VAD getInstance()  {

        if(sInstane == null){
            sInstane = new VAD();
        }
        return sInstane;
    }

    public static void waitForSpeech(){
        if(VAD.canCall()){
            baxterExecutor.broadcastToSpecificServer("STARTDETECTSPPECH#END", BaxterExecutor.PYTHON_SERVER_BAXTER);
            waitForSpeech = new WaitForSpeech(project, "HumanSpeaking", baxterMessageHandler);
            waitForSpeech.startListening();
        }

    }

    private static boolean canCall() {
        return project != null && baxterMessageHandler !=null && baxterExecutor != null;
    }

    public static void speechFinished(){
        if(VAD.canCall()){
            waitForSpeech.stopListening();
            baxterExecutor.broadcastToSpecificServer("STOPDETECTSPPECH#END", BaxterExecutor.PYTHON_SERVER_BAXTER);
        }
    }

    public static void setBaxterExecutor(BaxterExecutor b){
        baxterExecutor = b;
    }
}
