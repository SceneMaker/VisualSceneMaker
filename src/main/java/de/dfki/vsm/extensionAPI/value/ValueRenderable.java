package de.dfki.vsm.extensionAPI.value;

import javafx.scene.Node;

/**
 * Created by alvaro on 4/20/17.
 */
public interface ValueRenderable {
    void render();
    Node getRenderer();
    String getValue();

    void setValueProperty(ProjectValueProperty valueProperty);

    void setStyle();
}
