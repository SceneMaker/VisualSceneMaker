package de.dfki.vsm.xtesting.NewPropertyManager.model;

/**
 * Created by alvaro on 6/2/16.
 */
public abstract class AbstractTreeEntry {
    protected String name;
    public String getName(){
        return name;
    };

    public String toString(){
        return name;
    }

    public void setName(String pName){
        name = pName;
    }


}
