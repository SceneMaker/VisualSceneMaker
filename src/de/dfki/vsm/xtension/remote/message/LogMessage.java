/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remote.message;

/**
 *
 * @author Patrick
 * 
 */
public class LogMessage {
    public static enum Class {ACT, MESSAGE, SCENE, STATE, VARASSIGN, VARREQUEST};
    public static enum State {COMPLETED, CONTINUED};

    public static final String sID = "VSM";
    
    public Class mClass;
    public String mContent;
    public long mTimeStamp = -1;
    public long mDuration = -1;
    public State mState;
    
    public static final long SDEFAULTDURATION = 1000;
    
    public static final String sSEPARATOR = "#";
    
    public LogMessage() {
        //
    }
    
    public void setClass(Class c) {
        mClass = c;
    }

    public void setContent(String content) {
        mContent = content;
    }
    
    public void setTimeStamp(long timestamp) {
        mTimeStamp = timestamp;
    }

    public void setDuration(long duration) {
        mDuration = duration;
    }

    public void setState(State state) {
        mState = state;
    }
    
    public String toString() {
        StringBuilder sb = new StringBuilder();
        
        // build message according to format: <sender>#<class>#<content>#<timestamp>#<duration>#<state>
        // for example                       : VSM#SCENE#Welcome#123123123123123#5300#COMPLETED
        sb.append(sID).append(sSEPARATOR).append(mClass.name()).append(sSEPARATOR).append(mContent).append(sSEPARATOR).append(mTimeStamp).append(sSEPARATOR).append(mDuration).append(sSEPARATOR).append(mState.name());
        
        return sb.toString();
    }
}
