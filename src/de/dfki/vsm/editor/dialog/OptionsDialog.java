package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class OptionsDialog extends JDialog {
    
        private static OptionsDialog sSingeltonInstance = null;

    // private JComboBox mScenePlayerComboBox;
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final Editor           mEditor = Editor.getInstance();
    private JPanel                 mMainPanel;
    private JPanel                 mPrefPanel;

    // private JPanel mScenePlayerPanel;
    private JPanel      mButtonsPanel;
    private JPanel      mFileListPanel;
    private JList       mRecentFileList;
    private JScrollPane mRecentFileScrollPane;
    private JButton     mCancelButton;
    private JButton     mOkButton;
    private JLabel      mNodeSizeLabel;
    private JSpinner    mNodeSizeSpinner;
    private JLabel      mWorkspaceFontSizeLabel;
    private JSpinner    mWorkspaceFontSizeSpinner;
    private JLabel      mScriptFontSizeLabel;
    private JSpinner    mScriptFontSizeSpinner;
    private JLabel      mScriptFontTypeLabel;

    // private JSpinner mScriptFontTypeSpinner;
    private JComboBox                mScriptFontComboBox;
    private JLabel                   mGridScaleLabel;
    private JSpinner                 mGridScaleSpinner;
    private JCheckBox                mGridCheckBox;
    private JCheckBox                mVisualizationCheckBox;
    private JCheckBox                mVisualizationTraceCheckBox;
    private JCheckBox                mShowNodeIDCheckBox;
    private JCheckBox                mShowVariablesCheckBox;
    private JCheckBox                mShowSmartPathDebugCheckBox;
    private JPanel                   mGeneralPanel;
    private JLabel                   mXMLNSLabel;
    private JTextField               mXMLNSTextField;
    private JLabel                   mXMLInstanceLabel;
    private JTextField               mXMLInstanceTextField;
    private JLabel                   mXSDFileLabel;
    private JTextField               mXSDFileTextField;
    private JButton                  mXSDFileButton;
    private JPanel                   mGraphicsPanel;
    private JPanel                   mScriptPanel;
    private JButton                  mDeleteRecentFileListButton;
    private JButton                  mDeleteRecentFileButton;
    private ProjectPreferences       mPreferences;
    private ProjectData              mProject;

    private OptionsDialog() {
        super(Editor.getInstance(), "Preferences", false);
        initComponents();
        initPreferences();
    
    }

    public static OptionsDialog getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new OptionsDialog();
        }

        sSingeltonInstance.initPreferences();

        return sSingeltonInstance;
    }

    private void initComponents() {
        initGeneralPanel();
        initFileListPanel();
        initGraphicsPanel();
        initScriptPanel();

        // initScenePlayerPanel();
        mButtonsPanel = new JPanel();
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                savePreferences(true);
            }
        });
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                dispose();
            }
        });

        // Do the layout
        mPrefPanel = new JPanel();
        mPrefPanel.setLayout(new BoxLayout(mPrefPanel, BoxLayout.Y_AXIS));
        mPrefPanel.add(mGeneralPanel);

        // mPrefPanel.add(mFileListPanel);
        mPrefPanel.add(mGraphicsPanel);
        mPrefPanel.add(mScriptPanel);

        // mPrefPanel.add(mScenePlayerPanel);
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mCancelButton);
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mMainPanel = new JPanel();
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(mPrefPanel);
        mMainPanel.add(mButtonsPanel);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        pack();
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
    }

    private void initGeneralPanel() {
        mXMLNSLabel           = new JLabel("Namespace:");
        mXMLNSTextField       = new JTextField();
        mXMLInstanceLabel     = new JLabel("Instance:");
        mXMLInstanceTextField = new JTextField();
        mXSDFileLabel         = new JLabel("Location:");
        mXSDFileTextField     = new JTextField();
        mXSDFileTextField.setEditable(false);
        mXSDFileButton = new JButton("Select");
        mXSDFileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                JFileChooser file = new JFileChooser(mPreferences.sUSER_DIR);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(Editor.getInstance(), "Select Sceneflow XSD");

                if (file.getSelectedFile() != null) {
                    mXSDFileTextField.setText(file.getSelectedFile().getPath());
                }
            }
        });

        JPanel xmlNameSpace = new JPanel();

        xmlNameSpace.setLayout(new BoxLayout(xmlNameSpace, BoxLayout.X_AXIS));
        xmlNameSpace.add(Box.createRigidArea(new Dimension(5, 0)));
        xmlNameSpace.add(mXMLNSLabel);
        xmlNameSpace.add(Box.createHorizontalGlue());
        xmlNameSpace.add(mXMLNSTextField);
        xmlNameSpace.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel xmlInstance = new JPanel();

        xmlInstance.setLayout(new BoxLayout(xmlInstance, BoxLayout.X_AXIS));
        xmlInstance.add(Box.createRigidArea(new Dimension(5, 0)));
        xmlInstance.add(mXMLInstanceLabel);
        xmlInstance.add(Box.createHorizontalGlue());
        xmlInstance.add(mXMLInstanceTextField);
        xmlInstance.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel xsdFile = new JPanel();

        xsdFile.setLayout(new BoxLayout(xsdFile, BoxLayout.X_AXIS));
        xsdFile.add(Box.createRigidArea(new Dimension(5, 0)));
        xsdFile.add(mXSDFileLabel);
        xsdFile.add(Box.createHorizontalGlue());
        xsdFile.add(mXSDFileTextField);
        xsdFile.add(Box.createRigidArea(new Dimension(5, 0)));
        xsdFile.add(mXSDFileButton);
        xsdFile.add(Box.createRigidArea(new Dimension(5, 0)));
        mGeneralPanel = new JPanel();
        mGeneralPanel.setLayout(new BoxLayout(mGeneralPanel, BoxLayout.Y_AXIS));
        mGeneralPanel.setBorder(BorderFactory.createTitledBorder(" Sceneflow Syntax "));
        mGeneralPanel.add(xmlNameSpace);
        mGeneralPanel.add(xmlInstance);
        mGeneralPanel.add(xsdFile);
    }

    private void initFileListPanel() {
        mRecentFileList       = new JList(new DefaultListModel());
        mRecentFileScrollPane = new JScrollPane(mRecentFileList);
        mRecentFileScrollPane.setBounds(140, 95, 230, 100);
        mDeleteRecentFileButton = new JButton("Remove Item");
        mDeleteRecentFileButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                int index = mRecentFileList.getSelectedIndex();

                if (index >= 0) {
                    ((DefaultListModel) mRecentFileList.getModel()).remove(index);
                }
            }
        });
        mDeleteRecentFileListButton = new JButton("Delete List");
        mDeleteRecentFileListButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                ((DefaultListModel) mRecentFileList.getModel()).clear();
            }
        });

        // Do the layout - pack all stuff into little small cute boxes
        JPanel fileList = new JPanel();

        fileList.setLayout(new BoxLayout(fileList, BoxLayout.X_AXIS));
        fileList.add(Box.createRigidArea(new Dimension(5, 0)));
        fileList.add(mRecentFileScrollPane);
        fileList.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel recentFileButtons = new JPanel();

        recentFileButtons.setLayout(new BoxLayout(recentFileButtons, BoxLayout.X_AXIS));
        recentFileButtons.add(Box.createHorizontalGlue());
        recentFileButtons.add(mDeleteRecentFileButton);
        recentFileButtons.add(mDeleteRecentFileListButton);
        recentFileButtons.add(Box.createRigidArea(new Dimension(5, 0)));
        mFileListPanel = new JPanel();
        mFileListPanel.setLayout(new BoxLayout(mFileListPanel, BoxLayout.Y_AXIS));
        mFileListPanel.setBorder(BorderFactory.createTitledBorder(" Recently Edited Sceneflows "));
        mFileListPanel.add(fileList);
        mFileListPanel.add(recentFileButtons);
    }

    private void initGraphicsPanel() {
        mGridScaleLabel         = new JLabel("Grid Scale:");
        mWorkspaceFontSizeLabel = new JLabel("Font Size:");
        mGridCheckBox           = new JCheckBox("Draw Grid", true);
        mGridCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });

        // Node size stuff
        mNodeSizeLabel            = new JLabel("Node Size:");
        mNodeSizeSpinner          = new JSpinner(new SpinnerNumberModel(60, 20, 200, 2));
        mGridScaleSpinner         = new JSpinner(new SpinnerNumberModel(1, 1, 8, 1));
        mWorkspaceFontSizeSpinner = new JSpinner(new SpinnerNumberModel(10, 8, 16, 1));
        ((JSpinner.NumberEditor) mNodeSizeSpinner.getEditor()).getTextField().setEditable(false);
        ((JSpinner.NumberEditor) mGridScaleSpinner.getEditor()).getTextField().setEditable(false);
        mVisualizationCheckBox = new JCheckBox("Activitiy Visualization", true);
        mVisualizationCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
        mVisualizationTraceCheckBox = new JCheckBox("Activity Trace", true);
        mVisualizationTraceCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
        mShowNodeIDCheckBox = new JCheckBox("Draw Node ID", true);
        mShowNodeIDCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
        mShowVariablesCheckBox = new JCheckBox("Show Variables", true);
        mShowVariablesCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
        mShowSmartPathDebugCheckBox = new JCheckBox("Show Smart Path Calculation", false);
        mShowSmartPathDebugCheckBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });

        // Do the Layout - pack all stuff into little small cute boxesÏ
        JPanel fontAndSize = new JPanel();

        fontAndSize.setLayout(new BoxLayout(fontAndSize, BoxLayout.X_AXIS));
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(mNodeSizeLabel);
        fontAndSize.add(mNodeSizeSpinner);
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(Box.createHorizontalGlue());
        fontAndSize.add(mWorkspaceFontSizeLabel);
        fontAndSize.add(mWorkspaceFontSizeSpinner);
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(Box.createHorizontalGlue());
        fontAndSize.add(mGridScaleLabel);
        fontAndSize.add(mGridScaleSpinner);
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel drawOptions = new JPanel();

        drawOptions.setLayout(new BoxLayout(drawOptions, BoxLayout.Y_AXIS));
        drawOptions.add(mGridCheckBox);
        drawOptions.add(mShowNodeIDCheckBox);
        drawOptions.add(mShowVariablesCheckBox);
        drawOptions.add(mShowSmartPathDebugCheckBox);

        JPanel activityOptions = new JPanel();

        activityOptions.setLayout(new BoxLayout(activityOptions, BoxLayout.Y_AXIS));
        activityOptions.add(mVisualizationCheckBox);
        activityOptions.add(mVisualizationTraceCheckBox);

        JPanel graphicOptions = new JPanel();

        graphicOptions.setLayout(new BoxLayout(graphicOptions, BoxLayout.X_AXIS));
        graphicOptions.add(Box.createRigidArea(new Dimension(5, 0)));
        graphicOptions.add(drawOptions);
        graphicOptions.add(Box.createHorizontalGlue());
        graphicOptions.add(activityOptions);
        graphicOptions.add(Box.createRigidArea(new Dimension(5, 0)));
        mGraphicsPanel = new JPanel();
        mGraphicsPanel.setLayout(new BoxLayout(mGraphicsPanel, BoxLayout.Y_AXIS));
        mGraphicsPanel.setBorder(BorderFactory.createTitledBorder(" Visual Appearance "));
        mGraphicsPanel.add(fontAndSize);
        mGraphicsPanel.add(graphicOptions);
    }

    private void initScriptPanel() {
        mScriptFontTypeLabel = new JLabel("Font Type:");

        GraphicsEnvironment g            = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[]            fonts        = g.getAvailableFontFamilyNames();
        JPanel              controlPanel = new JPanel();

        mScriptFontComboBox = new JComboBox(fonts);
        mScriptFontComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
        mScriptFontSizeLabel   = new JLabel("Font Size:");
        mScriptFontSizeSpinner = new JSpinner(new SpinnerNumberModel(10, 8, 16, 1));

        // Do the Layout - pack all stuff into little small cute boxesÏ
        JPanel fontAndSize = new JPanel();

        fontAndSize.setLayout(new BoxLayout(fontAndSize, BoxLayout.X_AXIS));
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(Box.createHorizontalGlue());
        fontAndSize.add(mScriptFontTypeLabel);
        fontAndSize.add(Box.createHorizontalGlue());
        fontAndSize.add(mScriptFontComboBox);
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(mScriptFontSizeLabel);
        fontAndSize.add(Box.createHorizontalGlue());
        fontAndSize.add(mScriptFontSizeSpinner);
        fontAndSize.add(Box.createRigidArea(new Dimension(5, 0)));
        fontAndSize.add(Box.createHorizontalGlue());
        mScriptPanel = new JPanel();
        mScriptPanel.setLayout(new BoxLayout(mScriptPanel, BoxLayout.Y_AXIS));
        mScriptPanel.setBorder(BorderFactory.createTitledBorder(" Script Options "));
        mScriptPanel.add(fontAndSize);
    }

//  private void initScenePlayerPanel() {
//      mScenePlayerPanel = new JPanel();
//      mScenePlayerPanel.setLayout(new BoxLayout(mScenePlayerPanel, BoxLayout.X_AXIS));
//      mScenePlayerPanel.setBorder(BorderFactory.createTitledBorder(" Scene Player Preferences "));
//
//      mScenePlayerPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      mScenePlayerPanel.add(new JLabel("Model"));
//
//      mScenePlayerComboBox = new JComboBox(new DefaultComboBoxModel());
//      mScenePlayerPanel.add(mScenePlayerComboBox);
//      mScenePlayerPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//
//  }
    private void savePreferences(boolean dispose) {

        // mLogger.message("\r\nSaving Preferences:");
        mPreferences.setProperty("xmlns", mXMLNSTextField.getText().trim());
        mPreferences.setProperty("xmlns_xsi", mXMLInstanceTextField.getText().trim());
        mPreferences.setProperty("xsi_schemeLocation", mXSDFileTextField.getText().trim());
        mPreferences.setProperty(
            "node_width", Integer.toString(((SpinnerNumberModel) mNodeSizeSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty(
            "node_height", Integer.toString(((SpinnerNumberModel) mNodeSizeSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty(
            "grid_x", Integer.toString(((SpinnerNumberModel) mGridScaleSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty(
            "grid_y", Integer.toString(((SpinnerNumberModel) mGridScaleSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty(
            "workspace_fontsize",
            Integer.toString(((SpinnerNumberModel) mWorkspaceFontSizeSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty("grid", Boolean.toString(mGridCheckBox.isSelected()));
        mPreferences.setProperty("visualization", Boolean.toString(mVisualizationCheckBox.isSelected()));
        mPreferences.setProperty("visualizationtrace",
                                        Boolean.toString(mVisualizationTraceCheckBox.isSelected()));
        mPreferences.setProperty("shownodeid", Boolean.toString(mShowNodeIDCheckBox.isSelected()));
        mPreferences.setProperty("showvariables", Boolean.toString(mShowVariablesCheckBox.isSelected()));
        mPreferences.setProperty("showsmartpathcalculations",
                                        Boolean.toString(mShowSmartPathDebugCheckBox.isSelected()));
        mPreferences.setProperty(
            "scriptfonsize",
            Integer.toString(((SpinnerNumberModel) mScriptFontSizeSpinner.getModel()).getNumber().intValue()));
        mPreferences.setProperty("scriptfonttype", mScriptFontComboBox.getSelectedItem().toString());
        
//      Preferences.setProperty("selectedsceneplayer",
//              (String) mScenePlayerComboBox.getSelectedItem());
        // Write sceneplayers
//      int size = ((DefaultComboBoxModel) mScenePlayerComboBox.getModel()).getSize();
//      int i = 0;
//      for (i = 0; i < size; i++) {
//          String value = (String) ((DefaultComboBoxModel) mScenePlayerComboBox.getModel()).getElementAt(i);
//
//          if (value.trim().equalsIgnoreCase("")) {
//              Preferences.removeProperty("sceneplayer" + i);
//              mLogger.message("  Removing sceneplayer" + i + " ");
//          } else {
//              Preferences.setProperty("sceneplayer" + i, value);
//              mLogger.message("  Setting sceneplayer" + i + " to " + value);
//          }
//
//      }
//      for (; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
//          Preferences.removeProperty("sceneplayer" + i);
//          mLogger.message("  Removing sceneplayer" + i + " ");
//      }
//
        // Write recent file list
//      int size = ((DefaultListModel) mRecentFileList.getModel()).size();
//      int i = 0;
//      for (i = 0; i < size; i++) {
//          String value = (String) ((DefaultListModel) mRecentFileList.getModel()).get(i);
//          if (value.trim().equalsIgnoreCase("")) {
//              Preferences.removeProperty("recentfile" + i);
//              mLogger.message("  Removing recentfile" + i + " ");
//          } else {
//              Preferences.setProperty("recentfile" + i, value);
//              mLogger.message("  Setting recentfile" + i + " to " + value);
//          }
//
//      }
//      for (; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
//          Preferences.removeProperty("recentfile" + i);
//          mLogger.message("  Removing recentfile" + i + " ");
//      }
        mPreferences.save(mProject.getPreferencesFileName());

        if (dispose) {
            dispose();
        }
    }
    private void initPreferences() {
        
        mProject     = mEditor.getSelectedProjectEditor().getProject();
        mPreferences = mEditor.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().getPreferences();
        
        ((DefaultListModel) mRecentFileList.getModel()).clear();

//      ((DefaultComboBoxModel) mScenePlayerComboBox.getModel()).removeAllElements();

        for (Object keyObj : mPreferences.getKeySet()) {
            String key = (String) keyObj;

            if (key.startsWith("recentfile")) {
                ((DefaultListModel) mRecentFileList.getModel()).addElement(mPreferences.getProperty(key));
            }    // else if (key.startsWith("sceneplayer")) {

            // ((DefaultComboBoxModel) mScenePlayerComboBox.getModel()).addElement(Preferences.getProperty(key));
            // }
            else if (key.equals("node_width")) {
                ((SpinnerNumberModel) mNodeSizeSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("node_height")) {
                ((SpinnerNumberModel) mNodeSizeSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("grid_x")) {
                ((SpinnerNumberModel) mGridScaleSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("grid_y")) {
                ((SpinnerNumberModel) mGridScaleSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("grid")) {
                mGridCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("scriptfontype")) {
                mVisualizationCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("scriptfonsize")) {
                ((SpinnerNumberModel) mScriptFontSizeSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("scriptfonttype")) {
                mScriptFontComboBox.setSelectedItem(mPreferences.getProperty(key));
            } else if (key.equals("visualizationtrace")) {
                mVisualizationTraceCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("shownodeid")) {
                mShowNodeIDCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("showvariables")) {
                mShowVariablesCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("showsmartpathcalculations")) {
                mShowSmartPathDebugCheckBox.setSelected(Boolean.valueOf(mPreferences.getProperty(key)));
            } else if (key.equals("xmlns")) {
                mXMLNSTextField.setText(mPreferences.getProperty(key));
            } else if (key.equals("xmlns_xsi")) {
                mXMLInstanceTextField.setText(mPreferences.getProperty(key));
            } else if (key.equals("xsi_schemeLocation")) {
                mXSDFileTextField.setText(mPreferences.getProperty(key));
            } else if (key.equals("workspace_fontsize")) {
                ((SpinnerNumberModel) mWorkspaceFontSizeSpinner.getModel()).setValue(
                    Integer.valueOf(mPreferences.getProperty(key)));
            }

//          else if (key.equals("sceneplayer")) {
//              mScenePlayerComboBox.setSelectedItem(Preferences.getProperty(key));
//          }
        }

        // Add specific listeners
        mNodeSizeSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });

        // Add specific listeners
        mScriptFontSizeSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });

        // Add specific listeners
        mGridScaleSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });

        // Add specific listeners
        mWorkspaceFontSizeSpinner.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                savePreferences(false);
                mEditor.update();
            }
        });
    }
}

