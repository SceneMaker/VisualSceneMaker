/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.remotesender.util;

import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;

/**
 *
 * @author Hamza
 */
public class EmotionalModel {

    public static final String CONSTERNATION = "consternation";
    public static final String ANGER = "anger";
    public static float anger=0;
    public static float consternation =0;
    private static RunTimeProject project;

    public static enum Quadrant{FIRST,SECOND,THIRD,FOURTH};

    public static void setProject(RunTimeProject project){
        EmotionalModel.project = project;
    }
    
    public static void shiftAnger(){
        AbstractValue value= project.getValueOf(ANGER);
        float sitV = (float) value.getValue();
        if(anger < 0){
            anger*= (1-sitV);
        }else{
            anger*= sitV;
        }
         
        project.setVariable(ANGER, anger);
    }
    
    public static void shiftConsternation(){
        AbstractValue value= project.getValueOf(CONSTERNATION);
        float sitV = (float) value.getValue();
        if(consternation <0){
            consternation *=(1-sitV);
        }else{
            consternation *=sitV;
        }
        project.setVariable(CONSTERNATION, consternation);
    }
    
    public static void calculateQuadrant(){
        String quadrant = null;
        if(anger>0 && consternation >0){
            quadrant = "first";
        }else if(anger>0 && consternation <0){
            quadrant = "second";
        }else if(anger<0 && consternation <0){
            quadrant = "third";
        }else if(anger<0 && consternation >0){
            quadrant = "fourth";
        }
        project.setVariable("quadrant", quadrant);
    }
    
}
