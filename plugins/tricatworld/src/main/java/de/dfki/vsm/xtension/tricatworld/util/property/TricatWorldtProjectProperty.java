package de.dfki.vsm.xtension.tricatworld.util.property;

import de.dfki.vsm.model.project.property.ExportableProperties;
import de.dfki.vsm.model.project.property.ProjectProperty;
import de.dfki.vsm.model.project.property.value.ProjectValueProperty;
import de.dfki.vsm.model.project.property.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by alvaro on 4/26/17.
 */
public class TricatWorldtProjectProperty implements ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public TricatWorldtProjectProperty(){
        ProjectProperty usejpl = new ProjectProperty("usejpl");
        ProjectValueProperty usejplVP = new ProjectValueProperty(ValueTYPE.BOOLEAN, false);

        ProjectProperty useexe = new ProjectProperty("useexe");
        ProjectValueProperty useexeVP = new ProjectValueProperty(ValueTYPE.BOOLEAN, false);

        ProjectProperty tworlddir = new ProjectProperty("tworlddir");
        ProjectValueProperty tworlddirVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null);

        ProjectProperty tworldexe = new ProjectProperty("tworldexe", true);
        ProjectValueProperty tworldexeVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null, true);

        ProjectProperty tworldcmd = new ProjectProperty("tworldcmd", true);
        ProjectValueProperty tworldcmdVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null, true);

        ProjectProperty cactordir = new ProjectProperty("cactordir", true);
        ProjectValueProperty cactordirVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null, true);


        ProjectProperty cactorexe = new ProjectProperty("cactorexe", true);
        ProjectValueProperty cactorexeVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null, true);

        ProjectProperty cactorcmd = new ProjectProperty("cactorcmd", true);
        ProjectValueProperty cactorcmdVP = new ProjectValueProperty(ValueTYPE.FILEPATH, null, true);

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
