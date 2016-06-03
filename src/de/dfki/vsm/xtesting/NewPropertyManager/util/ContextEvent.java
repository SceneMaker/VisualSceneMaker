package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry;

/**
 * Created by alvaro on 5/14/16.
 */
public class ContextEvent extends NotificationObject {
    private String contextName;
    private String pluginName;
    private AbstractTreeEntry treeEntry;
    public ContextEvent(){
        super();

    }
    public ContextEvent(String name, String plugin){
        super();
        contextName = name;
        pluginName = plugin;
    }

    public ContextEvent(String name, String plugin, AbstractTreeEntry item){
        super();
        contextName = name;
        pluginName = plugin;
        treeEntry = item;
    }

    public AbstractTreeEntry getTreeEntry(){
        return treeEntry;
    }

    public String getContextName(){
        return contextName;
    }

    public String getPluginName() {
        return pluginName;
    }
}
