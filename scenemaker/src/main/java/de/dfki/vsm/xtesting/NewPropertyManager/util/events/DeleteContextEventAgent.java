package de.dfki.vsm.xtesting.NewPropertyManager.util.events;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry;

/**
 * Created by alvaro on 8/6/16.
 */
public class DeleteContextEventAgent extends NotificationObject {
    private AbstractTreeEntry treeEntry;

    public DeleteContextEventAgent(AbstractTreeEntry item){
        treeEntry = item;
    }

    public AbstractTreeEntry getTreeEntry() {
        return treeEntry;
    }


}
