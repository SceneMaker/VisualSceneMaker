package de.dfki.vsm.xtesting.propertymanager.util;

import de.dfki.vsm.xtesting.NewPropertyManager.model.AbstractTreeEntry;
import de.dfki.vsm.xtesting.NewPropertyManager.model.EntryRoot;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeCell;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.KeyCode;

import java.util.LinkedList;

public  class TreeCellImpl<AbstractTreeEntry> extends TreeCell<AbstractTreeEntry> implements TreeObservable{

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
        } else {
            if (isEditing()) {
                if (textField != null) {
                    textField.setText(getString());
                }
                setText(null);
                setGraphic(textField);
            }else {
                setText(getItem() == null ? "" : getItem().toString());
                setGraphic(getTreeItem().getGraphic());
                if (getTreeItem() instanceof AbstractTreeItem ) {
                    setContextMenu(((AbstractTreeItem) getTreeItem()).getMenu());
                }else{
                    setContextMenu(new ContextMenu());
                }
            }
        }
    }

    public void startEdit() {
        if (!(getTreeItem() instanceof AbstractTreeItem)) {//Not editable
            return;
        }

        super.startEdit();
        if (textField == null){
            createTextField();
        }
        setText(null);
        setGraphic(textField);
        textField.selectAll();
        if (getItem() == null) {
            editingNode = null;
        } else {
            editingNode =getItem().toString();
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
            if (t.getCode() == KeyCode.ENTER){
                commitEdit((AbstractTreeEntry) new EntryRoot(textField.getText()));
            } else if (t.getCode() == KeyCode.ESCAPE) {
                cancelEdit();
            }
        });
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
        for (TreeObserver observer:observers    ) {
            observer.update(new CellEvent(newValue, oldValue));
        }
    }

    @Override
    public String toString(){
        return getTreeItem().toString();
    }
}
