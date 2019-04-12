package de.dfki.vsm.editor.project.sceneflow.attributes;

import de.dfki.vsm.PreferencesDesktop;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.event.EventDispatcher;
import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;

import javax.swing.*;
import java.awt.*;

/**
 * @author Gregor Mehlmann
 */
abstract class AttributeEditor extends JPanel implements EventListener {

    // The maintained data model node
    protected BasicNode mDataNode;

    // The GUI components of the editor
    protected final DefaultListModel mListModel;
    protected final JList mList;
    //
    private final JPanel mEditorPanel;
    private final JPanel mButtonPanel;
    private final JButton mAddButton;
    private final JButton mRemoveButton;
    private final JButton mEditButton;
    private final JButton mUpButton;
    private final JButton mDownButton;
    private final JScrollPane mScrollPane;

    // Construct an attribute editor
    public AttributeEditor(String title) {

        // Init the attribute list
        mListModel = new DefaultListModel();
        mList = new JList(mListModel);
        mList.setCellRenderer(new StripedCellRenderer());
        mScrollPane = new JScrollPane(mList);
        mScrollPane.setMaximumSize(new Dimension(1000, 100));
        mScrollPane.setPreferredSize(new Dimension(200, 100));
        mScrollPane.setMinimumSize(new Dimension(200, 100));

        // Init the button panel
        mAddButton = new JButton(PreferencesDesktop.ICON_PLUS_STANDARD);
        mAddButton.setRolloverIcon(PreferencesDesktop.ICON_PLUS_ROLLOVER);
        mAddButton.setDisabledIcon(PreferencesDesktop.ICON_PLUS_DISABLED);
        mAddButton.setMaximumSize(new Dimension(22, 22));
        mAddButton.setPreferredSize(new Dimension(22, 22));
        mAddButton.setMinimumSize(new Dimension(22, 22));
        mAddButton.setOpaque(false);
        mAddButton.setContentAreaFilled(false);
        mAddButton.setFocusable(false);
        mAddButton.setBorder(BorderFactory.createEmptyBorder());
        mAddButton.addActionListener(e -> {
            add();
            EditorInstance.getInstance().refresh();
        });

        //
        mRemoveButton = new JButton(PreferencesDesktop.ICON_MINUS_STANDARD);
        mRemoveButton.setRolloverIcon(PreferencesDesktop.ICON_MINUS_ROLLOVER);
        mRemoveButton.setDisabledIcon(PreferencesDesktop.ICON_MINUS_DISABLED);
        mRemoveButton.setMinimumSize(new Dimension(22, 22));
        mRemoveButton.setMaximumSize(new Dimension(22, 22));
        mRemoveButton.setPreferredSize(new Dimension(22, 22));
        mRemoveButton.setOpaque(false);
        mRemoveButton.setContentAreaFilled(false);
        mRemoveButton.setFocusable(false);
        mRemoveButton.setBorder(BorderFactory.createEmptyBorder());
        mRemoveButton.addActionListener(e -> {
            remove();
            EditorInstance.getInstance().refresh();
        });

        //
        mEditButton = new JButton(PreferencesDesktop.ICON_EDIT_STANDARD);
        mEditButton.setRolloverIcon(PreferencesDesktop.ICON_EDIT_ROLLOVER);
        mEditButton.setMinimumSize(new Dimension(22, 22));
        mEditButton.setMaximumSize(new Dimension(22, 22));
        mEditButton.setPreferredSize(new Dimension(22, 22));
        mEditButton.setOpaque(false);
        mEditButton.setContentAreaFilled(false);
        mEditButton.setFocusable(false);
        mEditButton.setBorder(BorderFactory.createEmptyBorder());
        mEditButton.addActionListener(e -> {
            edit();
            EditorInstance.getInstance().refresh();
        });

        //
        mUpButton = new JButton(PreferencesDesktop.ICON_UP_STANDARD);
        mUpButton.setRolloverIcon(PreferencesDesktop.ICON_UP_ROLLOVER);
        mUpButton.setDisabledIcon(PreferencesDesktop.ICON_UP_DISABLED);
        mUpButton.setMinimumSize(new Dimension(20, 20));
        mUpButton.setMaximumSize(new Dimension(20, 20));
        mUpButton.setPreferredSize(new Dimension(20, 20));
        mUpButton.setOpaque(false);
        mUpButton.setContentAreaFilled(false);
        mUpButton.setFocusable(false);
        mUpButton.setBorder(BorderFactory.createEmptyBorder());
        mUpButton.addActionListener(e -> {
            up();
            EditorInstance.getInstance().refresh();
        });

        //
        mDownButton = new JButton(PreferencesDesktop.ICON_DOWN_STANDARD);
        mDownButton.setRolloverIcon(PreferencesDesktop.ICON_DOWN_ROLLOVER);
        mDownButton.setDisabledIcon(PreferencesDesktop.ICON_DOWN_DISABLED);
        mDownButton.setMinimumSize(new Dimension(22, 22));
        mDownButton.setMaximumSize(new Dimension(22, 22));
        mDownButton.setPreferredSize(new Dimension(22, 22));
        mDownButton.setOpaque(false);
        mDownButton.setContentAreaFilled(false);
        mDownButton.setFocusable(false);
        mDownButton.setBorder(BorderFactory.createEmptyBorder());
        mDownButton.addActionListener(e -> {
            down();
            EditorInstance.getInstance().refresh();
        });

        //
        mButtonPanel = new JPanel();
        mButtonPanel.setOpaque(false);
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.Y_AXIS));
        mButtonPanel.setBorder(BorderFactory.createEmptyBorder());
        mButtonPanel.add(mAddButton);
        mButtonPanel.add(mRemoveButton);
        mButtonPanel.add(mEditButton);
        mButtonPanel.add(mUpButton);
        mButtonPanel.add(mDownButton);

        // Init the editor panel
        mEditorPanel = new JPanel();
        mEditorPanel.setOpaque(false);
        mEditorPanel.setLayout(new BoxLayout(mEditorPanel, BoxLayout.X_AXIS));
        mEditorPanel.add(mScrollPane);
        mEditorPanel.add(mButtonPanel);

        // Init the attribute editor
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setOpaque(false);
        setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), title));
        add(mEditorPanel);
        add(Box.createRigidArea(new Dimension(20, 100)));

        // Add the attribute editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }
    //Removes addButton if is not needed
    public void disableAddButton(){
        mAddButton.setEnabled(false);
    }
    //Removes removeButton
    public void disableRemoveButton(){
        mRemoveButton.setEnabled(false);
    }
    //Removes editButton
    public void disableEditButton(){
        mEditButton.setEnabled(false);
    }
    //Removes removeButton
    public void disableUpDownButtons(){
        mUpButton.setEnabled(false);
        mDownButton.setEnabled(false);
    }
    
    protected abstract void add();

    protected abstract void edit();

    protected abstract void remove();

    protected abstract void up();

    protected abstract void down();

    private class StripedCellRenderer extends JLabel implements ListCellRenderer {

        public StripedCellRenderer() {
            setOpaque(true);
        }

        @Override
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                boolean cellHasFocus) {
            setText(value.toString());

            Color background;
            Color foreground;

            // check if this cell represents the current DnD drop location
            JList.DropLocation dropLocation = list.getDropLocation();

            if ((dropLocation != null) && !dropLocation.isInsert() && (dropLocation.getIndex() == index)) {
                background = Color.BLUE;
                foreground = Color.WHITE;

                // check if this cell is selected
            } else if (isSelected) {

                // background = Color.ORANGE;
                background = new Color(25, 33, 243, 200);
                foreground = Color.WHITE;

                // unselected, and not the DnD drop location
            } else {
                if (index % 2 == 0) {

                    // background = new Color(255, 240, 240);
                    background = Color.WHITE;
                    foreground = Color.BLACK;
                } else {
                    background = new Color(235, 235, 235, 127);

                    // background = new Color(240, 240, 255);
                    foreground = Color.BLACK;
                }
            }

            setBackground(background);
            setForeground(foreground);

            return this;
        }
    }
}
