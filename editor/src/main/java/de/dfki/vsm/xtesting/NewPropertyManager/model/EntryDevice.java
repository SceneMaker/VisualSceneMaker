package de.dfki.vsm.xtesting.NewPropertyManager.model;

import java.util.LinkedList;

/**
 * Created by alvaro on 6/2/16.
 */
public class EntryDevice extends AbstractTreeEntry {
    private LinkedList<EntryPlugin> plugins = new LinkedList<>();
    public EntryDevice(String pName){
        name = pName;
    }

    public void addPlugin(EntryPlugin agent){
        plugins.add(agent);
    }

    public LinkedList<EntryPlugin> getPlugins(){
        return plugins;
    }

}
