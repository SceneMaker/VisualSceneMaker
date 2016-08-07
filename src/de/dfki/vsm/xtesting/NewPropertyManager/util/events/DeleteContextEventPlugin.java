package de.dfki.vsm.xtesting.NewPropertyManager.util.events;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry;

/**
 * Created by alvaro on 8/6/16.
 */
public class DeleteContextEventPlugin extends NotificationObject {
    private AbstractTreeEntry treeEntry;
    public DeleteContextEventPlugin(AbstractTreeEntry item){
        treeEntry = item;
    }

    public AbstractTreeEntry getTreeEntry() {
        return treeEntry;
    }


}
