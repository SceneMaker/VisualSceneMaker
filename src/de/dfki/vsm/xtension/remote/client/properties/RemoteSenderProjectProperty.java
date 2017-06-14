package de.dfki.vsm.xtension.remote.client.properties;

import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.renderers.IntegerRenderer;
import de.dfki.vsm.util.extensions.renderers.SelectableRenderer;
import de.dfki.vsm.util.extensions.renderers.StringRender;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.extensions.value.ValueTYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Created by alvaro on 5/2/17.
 */
public class RemoteSenderProjectProperty implements ExportableProperties{
    HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public RemoteSenderProjectProperty(){
        ProjectProperty rHost = new ProjectProperty("rHost", true,
                "IP address to send data to.\n ");
        ProjectValueProperty rHostVP = new ProjectValueProperty(ValueTYPE.STRING,
                "127.0.0.1",
                new StringRender());

        ProjectProperty rPort = new ProjectProperty("rPort", true,
                "Port to send data to.\n ");
        ProjectValueProperty rPortVP = new ProjectValueProperty(ValueTYPE.NUMERIC,
                8100,
                new IntegerRenderer());

        ProjectProperty connectionType = new ProjectProperty("connection_type",
                "Type of connection to use");

        ProjectValueProperty connectionTypeVP = new ProjectValueProperty(ValueTYPE.LIST, "tcp/ip",
                new SelectableRenderer(),
                false,
                new ArrayList<>(Arrays.asList("tcp/ip", "udp")));

        exportableProperties.put(rHost, rHostVP);
        exportableProperties.put(rPort, rPortVP);
        exportableProperties.put(connectionType, connectionTypeVP);

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
