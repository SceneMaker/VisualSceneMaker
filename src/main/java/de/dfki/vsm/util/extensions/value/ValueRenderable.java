package de.dfki.vsm.util.extensions.value;

import javafx.scene.Node;
import javafx.scene.control.Control;

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
