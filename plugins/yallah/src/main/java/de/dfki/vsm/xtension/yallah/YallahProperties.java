package de.dfki.vsm.xtension.yallah;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.SelectableRenderer;
import de.dfki.vsm.extensionAPI.renderers.StringRender;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class YallahProperties implements ExportableProperties {

    public final static int WEBSOCKET_SERVER_PORT = 5000;

    public enum LaunchMode {
        None,
        App,
        WebPage
    }

    private final Map<ProjectProperty, ProjectValueProperty> mProperties = new HashMap<>();

    public YallahProperties() {

        //
        // Hot to launch the YALLAH application (if needed)
        ArrayList<String> mLaunchModes = new ArrayList<>();
        for(LaunchMode m: LaunchMode.values()) {
            mLaunchModes.add(m.toString()) ;
        }

        mProperties.put(
                new ProjectProperty("launchMode",
                        true,
                        "How to launch the Yallah client application"),
                new ProjectValueProperty(ValueTYPE.LIST,
                        mLaunchModes.get(0),
                        new SelectableRenderer(),
                        true,
                        mLaunchModes)
        ) ;

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
