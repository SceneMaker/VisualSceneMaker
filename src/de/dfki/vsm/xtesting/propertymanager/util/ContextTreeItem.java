package de.dfki.vsm.xtesting.propertymanager.util;

import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeCell;

import java.util.LinkedList;

/**
 * Created by alvaro on 5/14/16.
 */
public class ContextTreeItem extends AbstractTreeItem implements TreeObservable{
    private LinkedList<TreeObserver> observers = new LinkedList<>();
    private String contextValue = "Agent";
    private String pluginName;
    public ContextTreeItem(String name) {
        this.setValue(name);
    }

    @Override
    public ContextMenu getMenu(){
        if(this.getParent()!=null && this.getParent().getValue().equals("Devices")) {
            MenuItem addInbox = new MenuItem("Add new agent");
            addInbox.setOnAction(new EventHandler() {
                public void handle(Event t) {
                    BoxTreeItem newBox = new BoxTreeItem(contextValue);
                    getChildren().add(newBox);
                    notifyObserver();
                }
            });
            return new ContextMenu(addInbox);
        }
        return new ContextMenu();
    }


    @Override
    public void registerObserver(TreeObserver object) {
        observers.add(object);
    }

    @Override
    public void unregisterObserver(TreeObserver object) {
        observers.remove(object);
    }

    @Override
    public void notifyObserver() {
        for (TreeObserver observer:observers) {
            observer.update(new ContextEvent(contextValue, this.getValue().toString()));
        }
    }

    public String getPluginName() {
        return pluginName;
    }
}

class BoxTreeItem extends AbstractTreeItem{
    public BoxTreeItem(String name) {
        this.setValue(name);
    }

    @Override
    public ContextMenu getMenu() {
        return new ContextMenu(new MenuItem("testing"));
    }


}

