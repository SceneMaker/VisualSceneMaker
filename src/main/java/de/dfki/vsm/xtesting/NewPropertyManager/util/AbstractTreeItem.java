package de.dfki.vsm.xtesting.NewPropertyManager.util;

import javafx.scene.control.ContextMenu;
import javafx.scene.control.TreeItem;

/**
 * Created by alvaro on 5/14/16.
 */
public abstract class AbstractTreeItem<AbstractTreeEntry> extends TreeItem<AbstractTreeEntry> {
    public abstract ContextMenu getMenu();
}