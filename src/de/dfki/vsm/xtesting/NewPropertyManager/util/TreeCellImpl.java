package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryAgent;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryDevice;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryPlugin;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryRoot;
import de.dfki.vsm.xtesting.NewPropertyManager.util.events.CellEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.KeyCode;

import java.util.LinkedList;

public class TreeCellImpl<AbstractTreeEntry> extends TreeCell<AbstractTreeEntry> implements TreeObservable {

    private TextField textField;
    private String editingNode;
    private String oldValue;
    private String newValue;
    private LinkedList<TreeObserver> observers = new LinkedList<>();

    @Override
    public void updateItem(AbstractTreeEntry item, boolean empty) {

        super.updateItem(item, empty);

        if (empty) {
            setText(null);
            setGraphic(null);
        } else if (isEditing()) {
            if (textField != null) {
                textField.setText(getString());
            }
            setText(null);
            setGraphic(textField);
        } else {
            setText(getItem() == null ? "" : getItem().toString());
            setGraphic(getTreeItem().getGraphic());
            if (getTreeItem() instanceof AbstractTreeItem) {
                setContextMenu(((AbstractTreeItem) getTreeItem()).getMenu());
            } else {
                setContextMenu(new ContextMenu());
            }
        }
    }

    public void startEdit() {
        if (((getTreeItem().getValue()) instanceof EntryDevice)) {//Not editable
            return;
        }

        super.startEdit();
        if (textField == null) {
            createTextField();
        }
        setText(null);
        setGraphic(textField);
        textField.selectAll();
        if (getItem() == null) {
            editingNode = null;
        } else {
            editingNode = getItem().toString();
            oldValue = editingNode;
            System.out.println(oldValue);
        }
    }

    private String getString() {
        return getItem().toString();
    }

    private void createTextField() {
        textField = new TextField(getString());
        textField.setOnKeyReleased((KeyEvent t) -> {
            if (t.getCode() == KeyCode.ENTER) {
                AbstractTreeEntry entry = getEntry();
                String newTextValue = textField.getText();
                if (newTextValue.equals("")) {
                    cancelEdit();
                } else {
                    commitEdit(getEditedItemFactory(entry, newTextValue));
                }
            } else if (t.getCode() == KeyCode.ESCAPE) {
                cancelEdit();
            }
        });
    }

    private AbstractTreeEntry getEntry() {
        AbstractTreeEntry entry = null;
//        if(this.getTreeItem() instanceof  BoxTreeItem){
//            entry  = this.getTreeItem().getValue();
//        }else
//        {
//            entry = (AbstractTreeEntry) ((ContextTreeItem)this.getTreeItem()).getEntryItem();
//        }

        if (this.getTreeItem() instanceof BoxTreeItem) {
            entry = this.getTreeItem().getValue();
        } else if (this.getTreeItem() instanceof ContextTreeItem) {
            entry = (AbstractTreeEntry) ((ContextTreeItem) this.getTreeItem()).getEntryItem();
        } else {
            entry = (AbstractTreeEntry) this.getTreeItem().getValue();
        }
        return entry;
    }

    private AbstractTreeEntry getEditedItemFactory(AbstractTreeEntry entry, String text) {
        if (entry instanceof EntryAgent) {
            ((EntryAgent) entry).setName(text);
        }
        if (entry instanceof EntryPlugin) {
            ((EntryPlugin) entry).setName(text);
        }
        if (entry instanceof EntryRoot) {
            ((EntryRoot) entry).setName(text);
        }
        return entry;
    }

    @Override
    public void commitEdit(AbstractTreeEntry item) {
        // rename the file or directory
        super.commitEdit(item);
        if (editingNode != null) {
            System.out.println("Editted end");
            newValue = this.getString();
            notifyObserver();

        }

    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();
        setText(getString());
        setGraphic(null);
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
        for (TreeObserver observer : observers) {
            observer.update(new CellEvent(newValue, oldValue, (de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry) getItem()));
        }
    }

    @Override
    public String toString() {
        return getTreeItem().toString();
    }
}
