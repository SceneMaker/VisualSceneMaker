package de.dfki.vsm.xtension.remoteQuestionnaire;

import de.dfki.vsm.extensionAPI.ExportableProperties ;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;
import de.dfki.vsm.extensionAPI.renderers.StringRender;

import java.util.HashMap;
import java.util.Map;

public class RemQuestProperties implements ExportableProperties {

    public final static int WEBSOCKET_SERVER_PORT = 5002;

    private final Map<ProjectProperty, ProjectValueProperty> mProperties = new HashMap<>();

    public RemQuestProperties() {

        //
        // The WebSocket interface listening port
        mProperties.put(
                new ProjectProperty("port",
                        true,
                        "The port on which to listen the incoming WebSocket connections"),
                new ProjectValueProperty(ValueTYPE.STRING,
                        WEBSOCKET_SERVER_PORT,
                        new StringRender(),
                        true
                )
        ) ;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return mProperties;
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }

}
