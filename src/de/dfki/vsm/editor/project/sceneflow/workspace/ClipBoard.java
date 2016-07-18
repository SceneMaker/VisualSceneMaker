package de.dfki.vsm.editor.project.sceneflow.workspace;

import de.dfki.vsm.model.sceneflow.BasicNode;

import java.util.HashSet;

/**
 * Created by alvaro on 7/18/16.
 * The clipboard should be shared among the different projects
 *
 */

public  class ClipBoard extends HashSet<BasicNode> {
    private static ClipBoard sInstance;
    private ClipBoard(){}
    public static ClipBoard getsInstance(){
        if(sInstance == null){
            sInstance = new ClipBoard();
        }
        return sInstance;
    }
}
