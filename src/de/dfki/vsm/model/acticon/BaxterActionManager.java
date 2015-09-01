/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.model.acticon;

import de.dfki.baxter.BaxterPlayer;
import de.dfki.vsm.model.script.AbstractWord;
import de.dfki.vsm.model.script.ActionFeature;
import de.dfki.vsm.model.script.ActionObject;
import de.dfki.vsm.model.script.SceneWord;
import de.dfki.vsm.util.tts.I4GMaryClient;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.sound.sampled.UnsupportedAudioFileException;


/**
 *
 * @author alvaro
 */
public class BaxterActionManager {
    private AbstractWord mAction;
    private BaxterPlayer mBaxter;
    private static BaxterActionManager instance = new BaxterActionManager();
    private Queue<AbstractWord> actionQueue;
    private long previous_time = 0;
    private boolean actionDone = true;
    private Object monitor = new Object();
    
    public BaxterActionManager(){
        mAction = null;
        mBaxter = BaxterPlayer.getInstance();
        actionQueue = new LinkedList<AbstractWord>();
    }
    
    public BaxterActionManager(ActionObject action){
        mAction = action;
        mBaxter = BaxterPlayer.getInstance();
    }
    
    public void closeConnection(){
        mBaxter.closeBaxterConnection();
    }
    
    public static BaxterActionManager getInstance(){
      return instance;
   }
    
    public void setAction(AbstractWord action){
        mAction = action;
    }
    
    public void addAction(AbstractWord action){
        actionQueue.add(action);
        
    }
    
    public int executeAction(){
        
        if(mAction != null){
            String action = ((ActionObject)mAction).getName();
            this.executeMethod(mBaxter.getClass(), action);
        }
        return 0;
    }
    
    public void executeActionQueue(){
        Timer actionTimer = new Timer();
        System.out.println("Starting Baxter action");
        Iterator it = actionQueue.iterator();
        /*TTS */
        I4GMaryClient mary = I4GMaryClient.instance();
        while (it.hasNext()) {
            AbstractWord iterAction = (AbstractWord) it.next();
            if (iterAction instanceof SceneWord) {
                mary.addWord(((SceneWord)iterAction).getText());
            } else if(iterAction instanceof ActionObject){ 
                //Speak the phrase before the Baxter action
                previous_time += executeActionSpeak(mary, actionTimer);
                actionDone = false;
                
                this.executeAction((ActionObject)iterAction, previous_time); 
                //Blocking: This is the feature that indicates wether it waits for the executinon to end or not
                ActionFeature feature =  getFeature((ActionObject)iterAction, "blocking"); 
                if(feature != null && feature.getVal().equals("0")){
                    actionDone = true;
                }
                while(!actionDone) {
                    synchronized(monitor) {
                      try {
                        monitor.wait();
                      } catch(InterruptedException e) {}
                    }
                }
                actionDone = true;
            }
        }
        previous_time += executeActionSpeak(mary, actionTimer);
        actionQueue.clear();
        
    }
    
    private ActionFeature getFeature(ActionObject obj, String key){
        LinkedList features = obj.getFeatureList();
        Iterator it = features.iterator();
        boolean found = false;
        ActionFeature feat = null;
        while(it.hasNext() && !found){
            feat = (ActionFeature) it.next();
            if(feat.getKey().equals(key)){
                found = true;
            }
        }
        return feat;
    }
    
    private long executeActionSpeak(I4GMaryClient mary, Timer actionTimer){
        float current_time = 0;
        String text = "";
        long startTime = System.currentTimeMillis();
        Date actionExecutionTime = new Date(startTime);
        try {
            current_time =  mary.getPhraseTime();
            text = mary.getText();
            mary.clearWordList();
            startTime = System.currentTimeMillis();
            actionExecutionTime = new Date(startTime);
        } catch (UnsupportedAudioFileException ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (Exception ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
        actionExecutionTime = new Date(startTime + previous_time);
        System.out.println("Texto :" + text + " Comienza " + actionExecutionTime.toString() );
        actionTimer.schedule(new SpeakTask(mary,text), actionExecutionTime);
        return (long) current_time;
}
    
    class SpeakTask extends TimerTask {
    // GestureAction
    I4GMaryClient mary = null;
    String phrase = null;
    
    SpeakTask(I4GMaryClient ga, String text) {
      mary = ga;
      phrase = text;
    }
    
    public void run() {
      //System.out.println("GestureTask.run()");
      try {
        //Execute the
        System.out.println("Speaking, phrase: "+ mary.getText());
        mary.speak(phrase);
        } catch (UnsupportedAudioFileException ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InterruptedException ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        } catch (Exception ex) {
            Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
  }
    
    
    private void executeAction(ActionObject actionObject, float delayTime){
 
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            public void run() {
               String action = actionObject.getName();
                boolean result = executeMethod(mBaxter.getClass(), action);
                actionDone = true;
                synchronized(monitor) {
                    monitor.notifyAll();
                }
                System.out.println("Result " + result);
            }
        }, (long) delayTime);
    }
    
    private void speakAction(I4GMaryClient mary, float delayTime){
        
        Timer timer = new Timer();
        timer.schedule(new TimerTask() {
            public void run() {
                try {
                //Execute the
                System.out.println("Speaking, phrase: "+ mary.getText());
                mary.speak();
                } catch (UnsupportedAudioFileException ex) {
                    Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
                } catch (InterruptedException ex) {
                    Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
                } catch (Exception ex) {
                    Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }, (long) delayTime);
    }
    
    
    
    private boolean executeMethod(Class c, String action){
        
        
        for (Method method : c.getDeclaredMethods()) {
            String name = method.getName().toLowerCase();
            String methodName = "do"+action.toLowerCase();
            if(methodName.equals(name)){
                System.out.println(c.getName() + "." + method.getName());
                
                try {
                    method.invoke(mBaxter, "Baxter"); //TODO: Change later
               } catch (IllegalAccessException e) {
               
               } catch (IllegalArgumentException ex) {
                    Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
                } catch (InvocationTargetException ex) {
                    Logger.getLogger(BaxterActionManager.class.getName()).log(Level.SEVERE, null, ex);
                }
 
            }
        
        }
        return true;

        
    }

   
}
