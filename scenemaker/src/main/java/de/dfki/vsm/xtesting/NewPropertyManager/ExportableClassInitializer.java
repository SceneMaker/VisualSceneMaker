package de.dfki.vsm.xtesting.NewPropertyManager;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ExportableProperties;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.xtesting.NewPropertyManager.exceptions.NotExportableInterface;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryPlugin;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;

public class ExportableClassInitializer {
    private final RunTimeProject project;
    private final PluginConfig pluginConfig;
    private Class clazz;
    private Constructor constructor;
    private RunTimePlugin runTimePlugin;
    private ExportableProperties exportableProperties;

    public ExportableClassInitializer( RunTimeProject project, PluginConfig pluginConfig){
        this.project = project;
        this.pluginConfig = pluginConfig;
    }

    void initializeClass() throws ClassNotFoundException, NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException, NotExportableInterface {
        createConstructor();
        createRuntimePlugin();
    }

    public ExportableProperties getAsExportablePropertyClass() {
        return (ExportableProperties) runTimePlugin;
    }

    private void createRuntimePlugin() throws InstantiationException, IllegalAccessException, InvocationTargetException {
        runTimePlugin = (RunTimePlugin) constructor.newInstance(pluginConfig, project);
    }

    private void createConstructor() throws ClassNotFoundException, NoSuchMethodException, NotExportableInterface {
        String className = pluginConfig.getClassName();
        clazz = Class.forName(className);
        isImplementingExportableInterface();
        constructor = clazz.getConstructor(PluginConfig.class, RunTimeProject.class);
    }

    private void isImplementingExportableInterface() throws NotExportableInterface {
        Class[] interfaces = clazz.getInterfaces();
        boolean implementsInterface = false;
        for (Class implementedInterface: interfaces) {
            if(implementedInterface.getName().contains("ExportableProperties")){
                implementsInterface = true;
            }
        }
        if(!implementsInterface){
            throw new NotExportableInterface("This class does not implement ExportableProperties interface");
        }
    }
}