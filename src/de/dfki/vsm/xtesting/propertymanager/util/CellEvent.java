package de.dfki.vsm.xtesting.propertymanager.util;

/**
 * Created by alvaro on 5/14/16.
 */
public class CellEvent extends NotificationObject {
    private String oldValue;
    private String newValue;

    public CellEvent(String newV, String oldV){
        oldValue = oldV;
        newValue = newV;
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
