
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.acticon.ActiconAction;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListCellRenderer;
import javax.swing.WindowConstants;

/**
 * @author Patrick Gebahrd
 */
public class SceneActionDialog extends JDialog {
    private static SceneActionDialog sInstance = null;
    private final LOGDefaultLogger   mLogger   = LOGDefaultLogger.getInstance();
    private final EditorInstance             mEditor   = EditorInstance.getInstance();
    private JPanel                   mMainPanel;
    private JPanel                   mActionPanel;
    private JPanel                   mButtonsPanel;
    private JPanel                   mFileListPanel;
    private JButton                  mCancelButton;
    private JButton                  mOkButton;

    // data variables
    private HintTextField       mActionName;
    private HintTextField       mArgument;
    private JTextArea        mDocuTextArea;
    private DefaultListModel mListModel;
    private JList            mList;
    private JLabel           mPreviewLabel;

    SceneActionDialog() {
        super(EditorInstance.getInstance(), "Scene Action Dialog", false);
        EditorInstance.getInstance().addEscapeListener(this);
        initComponents();
    }

    public static SceneActionDialog getInstance() {
        if (sInstance == null) {
            sInstance = new SceneActionDialog();
        }

        return sInstance;
    }

    private void initComponents() {
        initActionPanel();
        mButtonsPanel = new JPanel();
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                saveActions();
                dispose();
            }
        });
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                dispose();
            }
        });
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mCancelButton);
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mMainPanel = new JPanel();
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(mActionPanel);
        mMainPanel.add(mButtonsPanel);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        pack();
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
    }

    private void initActionPanel() {
        mActionPanel = new JPanel();
        mActionPanel.setLayout(new BoxLayout(mActionPanel, BoxLayout.Y_AXIS));
        mActionPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel namePanel    = new JPanel();
        JPanel docPanel     = new JPanel();
        JPanel argsPanel    = new JPanel();
        JPanel argListPanel = new JPanel();
        JPanel argCmdPanel  = new JPanel();
        JPanel argAddPanel  = new JPanel();

        // name
        namePanel.setLayout(new BoxLayout(namePanel, BoxLayout.X_AXIS));
        namePanel.add(Box.createRigidArea(new Dimension(3, 0)));
        namePanel.add(new JLabel("Name"));
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mActionName = new HintTextField("Enter Name");
        mActionName.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
        mActionName.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                mPreviewLabel.setText("[" + mActionName.getText() + ((mListModel.isEmpty())
                        ? "]"
                        : " ...]"));
            }
        });
        namePanel.add(mActionName);
        namePanel.add(Box.createHorizontalGlue());
        namePanel.add(Box.createRigidArea(new Dimension(27, 0)));

        // docu
        docPanel.setLayout(new BoxLayout(docPanel, BoxLayout.Y_AXIS));

        JPanel docLabel = new JPanel();

        docLabel.setLayout(new BoxLayout(docLabel, BoxLayout.X_AXIS));
        docLabel.add(new JLabel("Documentation"));
        docLabel.add(Box.createHorizontalGlue());
        docPanel.add(docLabel);
        docPanel.add(Box.createRigidArea(new Dimension(0, 5)));

        JPanel docText = new JPanel();

        docText.setLayout(new BoxLayout(docText, BoxLayout.X_AXIS));
        mDocuTextArea = new JTextArea();
        mDocuTextArea.setMaximumSize(new Dimension(200, 60));
        mDocuTextArea.setPreferredSize(new Dimension(200, 60));
        mDocuTextArea.setMinimumSize(new Dimension(200, 60));
        mDocuTextArea.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
        mDocuTextArea.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
                    String text = mDocuTextArea.getText();

                    text.replaceAll("\n", " ");
                    text.replaceAll("\r", " ");
                    text.replaceAll("\t", " ");
                    mDocuTextArea.setText(text);
                }
            }
        });
        docText.add(mDocuTextArea);
        docText.add(Box.createHorizontalGlue());
        docPanel.add(docText);
        docPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));

        // args
        argsPanel.setLayout(new BoxLayout(argsPanel, BoxLayout.Y_AXIS));
        argListPanel.setLayout(new BoxLayout(argListPanel, BoxLayout.X_AXIS));
        mListModel = new DefaultListModel();
        mList      = new JList(mListModel);
        mList.setCellRenderer(new StripedCellRenderer());

        JScrollPane scrollPane = new JScrollPane(mList);

        scrollPane.setMaximumSize(new Dimension(200, 60));
        scrollPane.setPreferredSize(new Dimension(200, 60));
        scrollPane.setMinimumSize(new Dimension(200, 60));
        argListPanel.add(scrollPane);
        argListPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        argCmdPanel.setLayout(new BoxLayout(argCmdPanel, BoxLayout.Y_AXIS));

        JButton removeButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/minus.png"));

        removeButton.setMinimumSize(new Dimension(20, 20));
        removeButton.setMaximumSize(new Dimension(20, 20));
        removeButton.setPreferredSize(new Dimension(20, 20));
        removeButton.setOpaque(false);
        removeButton.setBorder(BorderFactory.createEmptyBorder());
        removeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {

                // System.out.println("removing argument");
                int index = mList.getSelectedIndex();

                if (index != -1) {
                    mListModel.removeElementAt(index);
                }

                mPreviewLabel.setText("[" + mActionName.getText() + ((mListModel.isEmpty())
                        ? "]"
                        : " ...]"));
            }
        });

        JButton editButton = new JButton(Preferences.ICON_EDIT_STANDARD);

        editButton.setMinimumSize(new Dimension(20, 20));
        editButton.setMaximumSize(new Dimension(20, 20));
        editButton.setPreferredSize(new Dimension(20, 20));
        editButton.setOpaque(false);
        editButton.setBorder(BorderFactory.createEmptyBorder());
        editButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {

                // System.out.println("Edit Argument()");
            }
        });
        argCmdPanel.add(removeButton);
        argCmdPanel.add(editButton);
        argCmdPanel.add(Box.createVerticalGlue());
        argListPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        argListPanel.add(argCmdPanel);
        argListPanel.add(Box.createHorizontalGlue());
        argAddPanel.setLayout(new BoxLayout(argAddPanel, BoxLayout.X_AXIS));

        JButton addButton = new JButton(Preferences.ICON_MINUS_STANDARD);

        addButton.setMinimumSize(new Dimension(20, 20));
        addButton.setMaximumSize(new Dimension(20, 20));
        addButton.setPreferredSize(new Dimension(20, 20));
        addButton.setOpaque(false);
        addButton.setBorder(BorderFactory.createEmptyBorder());
        addButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                mListModel.addElement(mArgument.getText());
                mArgument.setText("");
                mPreviewLabel.setText("[" + mActionName.getText() + ((mListModel.isEmpty())
                        ? "]"
                        : " ...]"));
            }
        });
        argAddPanel.add(Box.createRigidArea(new Dimension(3, 0)));
        mArgument = new HintTextField("Enter Argument");
        mArgument.setBorder(BorderFactory.createLineBorder(Color.GRAY, 1));
        mArgument.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
                    mListModel.addElement(mArgument.getText());
                    mArgument.setText("");
                    mPreviewLabel.setText("[" + mActionName.getText() + ((mListModel.isEmpty())
                            ? "]"
                            : " ...]"));
                }
            }
        });
        argAddPanel.add(mArgument);
        argAddPanel.add(Box.createRigidArea(new Dimension(4, 0)));
        argAddPanel.add(addButton);
        argAddPanel.add(Box.createHorizontalGlue());
        argAddPanel.add(Box.createRigidArea(new Dimension(3, 0)));

        JPanel argLabel = new JPanel();

        argLabel.setLayout(new BoxLayout(argLabel, BoxLayout.X_AXIS));
        argLabel.add(Box.createRigidArea(new Dimension(3, 0)));
        argLabel.add(new JLabel("Arguments"));
        argLabel.add(Box.createHorizontalGlue());
        argsPanel.add(argLabel);
        argsPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        argsPanel.add(argAddPanel);
        argsPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        argsPanel.add(argListPanel);

        // preview
        JPanel previewPanel = new JPanel();

        previewPanel.setLayout(new BoxLayout(previewPanel, BoxLayout.X_AXIS));
        previewPanel.add(Box.createRigidArea(new Dimension(3, 0)));
        mPreviewLabel = new JLabel(" ");
        previewPanel.add(mPreviewLabel);
        mPreviewLabel.setForeground(Color.RED.darker());
        mActionPanel.add(namePanel);
        mActionPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        mActionPanel.add(docPanel);
        mActionPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        mActionPanel.add(argsPanel);
        mActionPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        mActionPanel.add(previewPanel);
    }

    public void prepareNewAction() {
        mActionName.setText("");
        mDocuTextArea.setText("");
        mArgument.setText("");
        mListModel.clear();
    }

    public void editAction(ActiconAction a, int pos) {

        /*
         * mActionName.setText(a.getActionName());
         * if (a.() != null) {
         * mDocuTextArea.setText(a.getDocumentation());
         * }
         * mArgument.setText("");
         * mListModel.clear();
         * for (SM3SceneMember arg : a.getParamList()) {
         * mListModel.insertElementAt(arg.getText(), pos);
         * }
         */
    }

    private void saveActions() {
        ActiconConfig            asd  = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getActicon();
        ArrayList<ActionFeature> args = new ArrayList<ActionFeature>();

        for (Object o : mListModel.toArray()) {
            String argString = (String) o;
            String key       = argString;
            String value     = "";

            if (argString.contains("=")) {
                key   = argString.substring(0, argString.indexOf("="));
                value = argString.substring(argString.indexOf("=") + 1);
            }

            // SM3SceneMember arg = new SM3SceneMember(key, value, null);
            // args.add(arg);
        }

        // SM3SceneAction a = new SM3SceneAction("", mActionName.getText(), args, null);
        // asd.addAction(a);
        // TODO: Why do we save this here?
        //Editor.getInstance().getSelectedProjectEditor().getProject().saveActicon();
        EditorInstance.getInstance().refresh();
    }

    private class StripedCellRenderer extends JLabel implements ListCellRenderer {
        public StripedCellRenderer() {
            setOpaque(true);
        }

        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                boolean cellHasFocus) {
            setText(value.toString());

            Color background;
            Color foreground;

            // check if this cell represents the current DnD drop location
            JList.DropLocation dropLocation = list.getDropLocation();

            if ((dropLocation != null) &&!dropLocation.isInsert() && (dropLocation.getIndex() == index)) {
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
