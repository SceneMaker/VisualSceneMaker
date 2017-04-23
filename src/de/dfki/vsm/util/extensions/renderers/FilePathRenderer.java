package de.dfki.vsm.util.extensions.renderers;

import de.dfki.vsm.util.extensions.renderers.customcontrollers.CustomDirectoryChooser;
import de.dfki.vsm.util.extensions.value.ValueRenderable;

/**
 * Created by alvaro on 4/20/17.
 */
public class FilePathRenderer extends ValueRender {
    String value;
    private CustomDirectoryChooser directoryChooser;

    @Override
    public void render() {
        directoryChooser = new CustomDirectoryChooser();
        control = directoryChooser.getControl();
    }

    @Override
    public String getValue() {
        return directoryChooser.getFilePath();
    }


}
