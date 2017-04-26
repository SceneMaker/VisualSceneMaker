package de.dfki.vsm.util.extensions.renderers;

import de.dfki.vsm.util.extensions.renderers.customcontrollers.CustomPathChooser;
import de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers.PathChooser;

/**
 * Created by alvaro on 4/20/17.
 */
public class FilePathRenderer extends ValueRender {
    String value;
    private CustomPathChooser directoryChooser;

    public FilePathRenderer(){
        directoryChooser = new CustomPathChooser();
    }
    public FilePathRenderer(PathChooser pathChooser){
        directoryChooser = new CustomPathChooser(pathChooser);
    }

    @Override
    public void render() {

        control = directoryChooser.getControl();
    }

    @Override
    public String getValue() {
        return directoryChooser.getFilePath();
    }


}
