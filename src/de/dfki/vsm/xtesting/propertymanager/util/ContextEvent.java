package de.dfki.vsm.xtesting.propertymanager.util;

/**
 * Created by alvaro on 5/14/16.
 */
public class ContextEvent extends NotificationObject {
    private String contextName;
    private String pluginName;
    public ContextEvent(){
        super();

    }
    public ContextEvent(String name, String plugin){
        super();
        contextName = name;
        pluginName = plugin;
    }

    public String getContextName(){
        return contextName;
    }

    public String getPluginName() {
        return pluginName;
    }
}
