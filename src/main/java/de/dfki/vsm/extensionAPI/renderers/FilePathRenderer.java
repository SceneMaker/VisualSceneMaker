package de.dfki.vsm.extensionAPI.renderers;

import de.dfki.vsm.extensionAPI.renderers.customcontrollers.CustomPathChooser;
import de.dfki.vsm.extensionAPI.renderers.customcontrollers.pathchoosers.PathChooser;

/**
 * Created by alvaro on 4/20/17.
 */
public class FilePathRenderer extends ValueRender {
    String value;
    private CustomPathChooser pathChooser;

    public FilePathRenderer(){
        pathChooser = new CustomPathChooser();
    }
    public FilePathRenderer(PathChooser pathChooser){
        this.pathChooser = new CustomPathChooser(pathChooser);
    }

    @Override
    public void render() {

        control = pathChooser.getControl();
    }

    @Override
    public String getValue() {
        return pathChooser.getFilePath();
    }


}
