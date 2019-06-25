package de.dfki.vsm.xtension.pepper;

import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.renderers.StringRender;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.extensionAPI.value.ValueTYPE;

import java.util.HashMap;
import java.util.Map;


/**
 * Defines the properties available in the extension's menu within the VSM settings.
 *
 * @see <a href="http://scenemaker.dfki.de/tutorials/tut_showParametersInPropertyEditor.html">tutorial</a>
 */
public class PepperExtensionProperties implements ExportableProperties {

    private HashMap<ProjectProperty, ProjectValueProperty> exportableProperties = new HashMap<>();

    public PepperExtensionProperties() {
        // tutorial: http://scenemaker.dfki.de/tutorials/tut_showParametersInPropertyEditor.html

        /* example:
        // can have a description, can be required
        ProjectProperty myProperty = new ProjectProperty("My Parameter");
        // see ValueTYPE for available types
        ProjectValueProperty myPropertyValue = new ProjectValueProperty(ValueTYPE.FILEPATH, false,
                new FilePathRenderer());
        exportableProperties.put(myProperty, myPropertyValue);
        // see also CharamelProjectProperty.java
        */

        ProjectProperty python2Exe = new ProjectProperty("Python 2.7 (32 bit) executable", true,
                "Can be either a (fully specified) path or an alias available to the default command line environment\n" +
                        "Examples:\n1. Full path to python executable: C:/Python/python.exe\n" +
                        "2. Alias for the python executable (e.g. a symbolic link) on the path of the `cmd` utility: python2-32");
        ProjectValueProperty python2ExeValue = new ProjectValueProperty(ValueTYPE.STRING, "", new StringRender());
        exportableProperties.put(python2Exe, python2ExeValue);

        ProjectProperty robotIP = new ProjectProperty("Pepper IP address", "IP address of the Pepper robot, " +
                "default: pepper.local");
        ProjectValueProperty robotIPValue = new ProjectValueProperty(ValueTYPE.STRING, "pepper.local", new StringRender());
        exportableProperties.put(robotIP, robotIPValue);

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
