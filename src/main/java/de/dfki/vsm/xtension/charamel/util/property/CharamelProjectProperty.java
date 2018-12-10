package de.dfki.vsm.xtension.charamel.util.property;

import de.dfki.vsm.xtension.tricatworld.util.property.*;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.renderers.BooleanRenderer;
import de.dfki.vsm.util.extensions.renderers.FilePathRenderer;
import de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers.CustomFileChooser;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.extensions.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by alvaro on 4/26/17.
 */
public class CharamelProjectProperty implements ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public CharamelProjectProperty(){
        ProjectProperty usejpl = new ProjectProperty("usejpl");
        ProjectValueProperty usejplVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());

        ProjectProperty useexe = new ProjectProperty("useexe");
        ProjectValueProperty useexeVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());

        ProjectProperty tworlddir = new ProjectProperty("tworlddir");
        ProjectValueProperty tworlddirVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer());

        ProjectProperty tworldexe = new ProjectProperty("tworldexe", true);
        ProjectValueProperty tworldexeVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer(new CustomFileChooser()));

        ProjectProperty tworldcmd = new ProjectProperty("tworldcmd", true);
        ProjectValueProperty tworldcmdVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer(new CustomFileChooser()));

        ProjectProperty cactordir = new ProjectProperty("cactordir", true);
        ProjectValueProperty cactordirVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer());


        ProjectProperty cactorexe = new ProjectProperty("cactorexe", true);
        ProjectValueProperty cactorexeVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer(new CustomFileChooser()));

        ProjectProperty cactorcmd = new ProjectProperty("cactorcmd", true);
        ProjectValueProperty cactorcmdVP = new ProjectValueProperty(ValueTYPE.FILEPATH,
                false,
                new FilePathRenderer(new CustomFileChooser()));

        exportableProperties.put(useexe, useexeVP);
        exportableProperties.put(usejpl, usejplVP);
        exportableProperties.put(tworlddir, tworlddirVP);
        exportableProperties.put(tworldcmd, tworldcmdVP);
        exportableProperties.put(tworldexe, tworldexeVP);
        exportableProperties.put(cactorcmd, cactorcmdVP);
        exportableProperties.put(cactorexe, cactorexeVP);
        exportableProperties.put(cactordir, cactordirVP);


    }
    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
