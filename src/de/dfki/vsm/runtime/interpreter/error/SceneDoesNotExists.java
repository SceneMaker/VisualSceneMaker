package de.dfki.vsm.runtime.interpreter.error;

public class SceneDoesNotExists extends Exception  {
    // Create an interpret exception
    public SceneDoesNotExists(String name){
        super("Scene: " + name + " does not exists");
    }

}
