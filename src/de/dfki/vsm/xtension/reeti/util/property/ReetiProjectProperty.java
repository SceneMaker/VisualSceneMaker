package de.dfki.vsm.xtension.reeti.util.property;

import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.renderers.BooleanRenderer;
import de.dfki.vsm.util.extensions.renderers.IntegerRenderer;
import de.dfki.vsm.util.extensions.renderers.StringRender;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.extensions.value.ValueTYPE;

import java.util.HashMap;

/**
 * Created by alvaro on 4/24/17.
 */
public class ReetiProjectProperty implements ExportableProperties {
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public ReetiProjectProperty(){
        ProjectProperty lhost = new ProjectProperty("lhost", true);
        ProjectValueProperty lhostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "127.0.0.1",
                new StringRender());

        ProjectProperty lport = new ProjectProperty("lport", true);
        ProjectValueProperty lportVP= new ProjectValueProperty(ValueTYPE.NUMERIC,
                1421,
                new IntegerRenderer());


        ProjectProperty rhost = new ProjectProperty("rhost", true);
        ProjectValueProperty rhostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "127.0.0.1",
                new StringRender());

        ProjectProperty rport = new ProjectProperty("rport", true);
        ProjectValueProperty rportVP= new ProjectValueProperty(ValueTYPE.NUMERIC,
                1241,
                new IntegerRenderer());

        ProjectProperty useSSI = new ProjectProperty("useSSI");
        ProjectValueProperty useSSIVP = new ProjectValueProperty(ValueTYPE.BOOLEAN,
                false,
                new BooleanRenderer());

        exportableProperties.put(lhost, lhostVP);
        exportableProperties.put(rhost, rhostVP);
        exportableProperties.put(lport, lportVP);
        exportableProperties.put(rport, rportVP);
        exportableProperties.put(useSSI, useSSIVP);
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties;
    }

    @Override
    public HashMap<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}
