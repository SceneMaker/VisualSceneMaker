package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry;

/**
 * Created by alvaro on 5/14/16.
 */
public class CellEvent extends NotificationObject {
    private String oldValue;
    private String newValue;
    private AbstractTreeEntry treeEntry;

    public CellEvent(String newV, String oldV){
        oldValue = oldV;
        newValue = newV;
    }

    public CellEvent(String newV, String oldV, AbstractTreeEntry item){
        oldValue = oldV;
        newValue = newV;
        treeEntry = item;
    }

    public AbstractTreeEntry getTreeEntry(){
        return treeEntry;
    }

    public String getOldValue() {
        return oldValue;
    }

    public void setOldValue(String oldValue) {
        this.oldValue = oldValue;
    }

    public String getNewValue() {
        return newValue;
    }

    public void setNewValue(String newValue) {
        this.newValue = newValue;
    }
}
