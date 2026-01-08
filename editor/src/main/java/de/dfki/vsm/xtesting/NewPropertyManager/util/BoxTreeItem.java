package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryAgent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;

/**
 * Created by alvaro on 6/4/16.
 */
public class BoxTreeItem<AbstractTreeEntry> extends AbstractTreeItem{


    public BoxTreeItem(EntryAgent entryAgent) {
        super();
        this.setValue(entryAgent);
    }

    @Override
    public ContextMenu getMenu() {
        return new ContextMenu(new MenuItem("testing"));
    }

}




