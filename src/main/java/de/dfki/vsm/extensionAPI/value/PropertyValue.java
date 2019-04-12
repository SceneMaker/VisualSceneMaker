package de.dfki.vsm.extensionAPI.value;

/**
 * Created by alvaro on 4/20/17.
 */
public interface PropertyValue {
    void render();
    boolean validate();
    String getValue();
}
