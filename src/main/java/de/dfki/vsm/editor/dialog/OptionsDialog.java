package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.event.ProjectChangedEvent;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;

import static de.dfki.vsm.Preferences.sUSER_DIR;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class OptionsDialog extends JDialog {

    private static OptionsDialog sSingeltonInstance = null;

    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EditorInstance mEditor = EditorInstance.getInstance();
    private final Dimension mLabelDimension = new Dimension(100, 10);
    private final Dimension buttonSize = new Dimension(125, 30);
    private final Dimension textfieldSize = new Dimension(150, 30);
    private JPanel mMainPanel;
    private JPanel mPrefPanel;

    private JPanel mButtonsPanel;
    private JPanel mFileListPanel;
    private JList mRecentFileList;
    private JScrollPane mRecentFileScrollPane;
    private CancelButton mCancelButton;
    private OKButton mOkButton;
    private JLabel mNodeSizeLabel;
    private JSpinner mNodeSizeSpinner;
    private JLabel mWorkspaceFontSizeLabel;
    private JSpinner mWorkspaceFontSizeSpinner;
    private JLabel mScriptFontSizeLabel;
    private JSpinner mScriptFontSizeSpinner;
    private JLabel mScriptFontTypeLabel;

    // private JSpinner mScriptFontTypeSpinner;
    private JComboBox mScriptFontComboBox;
    private JLabel mGridScaleLabel;
    private JSpinner mGridScaleSpinner;
    private JCheckBox mLaunchDefaultPlayerCheckBox;
    private JCheckBox mGridCheckBox;
    private JCheckBox mVisualizationCheckBox;
    private JCheckBox mVisualizationTraceCheckBox;
    private JCheckBox mShowNodeIDCheckBox;
    private JCheckBox mShowVariablesCheckBox;
    private JCheckBox mShowSmartPathDebugCheckBox;
    private JPanel mGeneralPanel;
    private JLabel mXMLNSLabel;
    private JTextField mXMLNSTextField;
    private JLabel mXMLInstanceLabel;
    private JTextField mXMLInstanceTextField;
    private JLabel mXSDFileLabel;
    private JTextField mXSDFileTextField;
    private JButton mXSDFileButton;
    private JPanel mGraphicsPanel;
    private JPanel mScriptPanel;
    private JButton mDeleteRecentFileListButton;
    private JButton mDeleteRecentFileButton;

    private final EditorConfig mEditorConfig;

    private OptionsDialog() {
        super(EditorInstance.getInstance(), "Preferences", false);
        EditorInstance.getInstance().addEscapeListener(this);
        mEditorConfig = mEditor.getSelectedProjectEditor().getEditorProject().getEditorConfig();
        initComponents();
        initEditorConfig();

    }

    public static OptionsDialog getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new OptionsDialog();
        }

        sSingeltonInstance.initEditorConfig();

        return sSingeltonInstance;
    }

    private void initComponents() {
        initGeneralPanel();
        initFileListPanel();
        initGraphicsPanel();
        initScriptPanel();
        setBackground(Color.white);

        // initScenePlayerPanel();
        mButtonsPanel = new JPanel();
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mButtonsPanel.setOpaque(false);
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                saveEditorConfig(true);
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                dispose();
            }
        });

        // Do the layout
        mPrefPanel = new JPanel();
        mPrefPanel.setLayout(new BoxLayout(mPrefPanel, BoxLayout.Y_AXIS));
        //mPrefPanel.add(mGeneralPanel);

        // mPrefPanel.add(mFileListPanel);
        mPrefPanel.add(mGraphicsPanel);
        mPrefPanel.add(mScriptPanel);

        // mPrefPanel.add(mScenePlayerPanel);
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mCancelButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(40, 60)));
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mMainPanel = new JPanel();
        mMainPanel.setBackground(Color.white);
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(mPrefPanel);
        mMainPanel.add(mButtonsPanel);

        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(ke -> {
            //boolean keyHandled = false;
            if (ke.getID() == KeyEvent.KEY_PRESSED) {
                if (ke.getKeyCode() == KeyEvent.VK_ENTER) {
                    saveEditorConfig(true);
                }
            }
            return false;
        });
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        pack();
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
    }

    private void initGeneralPanel() {
        mXMLNSLabel = new JLabel("Namespace:");
        mXMLNSLabel.setMinimumSize(mLabelDimension);
        mXMLNSLabel.setPreferredSize(mLabelDimension);
        mXMLNSTextField = new JTextField();
        mXMLNSTextField.setMinimumSize(textfieldSize);
        mXMLNSTextField.setPreferredSize(textfieldSize);
        mXMLInstanceLabel = new JLabel("Instance:");
        mXMLInstanceLabel.setMinimumSize(mLabelDimension);
        mXMLInstanceLabel.setPreferredSize(mLabelDimension);
        mXMLInstanceTextField = new JTextField();
        mXMLInstanceTextField.setMinimumSize(textfieldSize);
        mXMLInstanceTextField.setPreferredSize(textfieldSize);
        mXSDFileLabel = new JLabel("Location:");
        mXSDFileLabel.setMinimumSize(mLabelDimension);
        mXSDFileLabel.setPreferredSize(mLabelDimension);
        mXSDFileTextField = new JTextField();
        mXSDFileTextField.setMinimumSize(textfieldSize);
        mXSDFileTextField.setPreferredSize(textfieldSize);
        mXSDFileTextField.setEditable(false);
        mXSDFileButton = new JButton(ResourceLoader.loadImageIcon("/res/img/search_icon.png"));
        mXSDFileButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/search_icon_blue.png"));
        mXSDFileButton.setOpaque(false);
        mXSDFileButton.setContentAreaFilled(false);
        mXSDFileButton.addActionListener(e -> {
            JFileChooser file = new JFileChooser(sUSER_DIR);

            file.setFileSelectionMode(JFileChooser.FILES_ONLY);
            file.showDialog(EditorInstance.getInstance(), "Select Sceneflow XSD");

            if (file.getSelectedFile() != null) {
                mXSDFileTextField.setText(file.getSelectedFile().getPath());
            }
        });

        JPanel xmlNameSpace = new JPanel();

        xmlNameSpace.setOpaque(false);
        xmlNameSpace.setLayout(new BoxLayout(xmlNameSpace, BoxLayout.X_AXIS));
        xmlNameSpace.add(Box.createRigidArea(new Dimension(5, 0)));
        xmlNameSpace.add(mXMLNSLabel);
        xmlNameSpace.add(Box.createHorizontalGlue());
        xmlNameSpace.add(mXMLNSTextField);
        xmlNameSpace.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel xmlInstance = new JPanel();

        xmlInstance.setOpaque(false);
        xmlInstance.setLayout(new BoxLayout(xmlInstance, BoxLayout.X_AXIS));
        xmlInstance.add(Box.createRigidArea(new Dimension(5, 0)));
        xmlInstance.add(mXMLInstanceLabel);
        xmlInstance.add(Box.createHorizontalGlue());
        xmlInstance.add(mXMLInstanceTextField);
        xmlInstance.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel xsdFile = new JPanel();

        xsdFile.setOpaque(false);
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
        mGeneralPanel.setBackground(Color.white);
        mGeneralPanel.setBorder(BorderFactory.createTitledBorder(" Sceneflow Syntax "));
        mGeneralPanel.add(Box.createRigidArea(new Dimension(1, 20)));
        mGeneralPanel.add(xmlNameSpace);
        mGeneralPanel.add(Box.createRigidArea(new Dimension(1, 10)));
        mGeneralPanel.add(xmlInstance);
        mGeneralPanel.add(Box.createRigidArea(new Dimension(1, 10)));
        mGeneralPanel.add(xsdFile);
        mGeneralPanel.add(Box.createRigidArea(new Dimension(1, 20)));
    }

    private void initFileListPanel() {
        mRecentFileList = new JList(new DefaultListModel());
        mRecentFileList.setOpaque(false);
        mRecentFileScrollPane = new JScrollPane(mRecentFileList);
        mRecentFileScrollPane.setOpaque(false);
        mRecentFileScrollPane.setBounds(140, 95, 230, 100);
        mDeleteRecentFileButton = new JButton("Remove Item");
        mDeleteRecentFileButton.addActionListener(e -> {
            int index = mRecentFileList.getSelectedIndex();

            if (index >= 0) {
                ((DefaultListModel) mRecentFileList.getModel()).remove(index);
            }
        });
        mDeleteRecentFileListButton = new JButton("Delete List");
        mDeleteRecentFileListButton.addActionListener(e -> ((DefaultListModel) mRecentFileList.getModel()).clear());

        // Do the layout - pack all stuff into little small cute boxes
        JPanel fileList = new JPanel();

        fileList.setOpaque(false);
        fileList.setLayout(new BoxLayout(fileList, BoxLayout.X_AXIS));
        fileList.add(Box.createRigidArea(new Dimension(5, 0)));
        fileList.add(mRecentFileScrollPane);
        fileList.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel recentFileButtons = new JPanel();

        recentFileButtons.setOpaque(false);
        recentFileButtons.setLayout(new BoxLayout(recentFileButtons, BoxLayout.X_AXIS));
        recentFileButtons.add(Box.createHorizontalGlue());
        recentFileButtons.add(mDeleteRecentFileButton);
        recentFileButtons.add(mDeleteRecentFileListButton);
        recentFileButtons.add(Box.createRigidArea(new Dimension(5, 0)));
        mFileListPanel = new JPanel();
        mFileListPanel.setBackground(Color.white);
        mFileListPanel.setLayout(new BoxLayout(mFileListPanel, BoxLayout.Y_AXIS));
        mFileListPanel.setBorder(BorderFactory.createTitledBorder(" Recently Edited Sceneflows "));
        mFileListPanel.add(fileList);
        mFileListPanel.add(recentFileButtons);
    }

    private void initGraphicsPanel() {
        mGridScaleLabel = new JLabel("Grid Scale:");
        mWorkspaceFontSizeLabel = new JLabel("Font Size:");
        mGridCheckBox = new JCheckBox("Draw Grid", true);
        mGridCheckBox.setOpaque(false);
        mGridCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });

        // Node size stuff
        mNodeSizeLabel = new JLabel("Node Size:");
        mNodeSizeSpinner = new JSpinner(new SpinnerNumberModel(mEditorConfig.sNODEHEIGHT, 20, 200, 2));
        mGridScaleSpinner = new JSpinner(new SpinnerNumberModel(mEditorConfig.sGRID_YSCALE, 1, 8, 1));
        mWorkspaceFontSizeSpinner = new JSpinner(new SpinnerNumberModel(mEditorConfig.sWORKSPACEFONTSIZE, 8, 16, 1));
        ((JSpinner.NumberEditor) mNodeSizeSpinner.getEditor()).getTextField().setEditable(false);
        ((JSpinner.NumberEditor) mGridScaleSpinner.getEditor()).getTextField().setEditable(false);
        mVisualizationCheckBox = new JCheckBox("Activitiy Visualization", true);
        mVisualizationCheckBox.setOpaque(false);
        mVisualizationCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
        mVisualizationTraceCheckBox = new JCheckBox("Activity Trace", true);
        mVisualizationTraceCheckBox.setOpaque(false);
        mVisualizationTraceCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
        mShowNodeIDCheckBox = new JCheckBox("Draw Node ID", true);
        mShowNodeIDCheckBox.setOpaque(false);
        mShowNodeIDCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
        mShowVariablesCheckBox = new JCheckBox("Show Variables", true);
        mShowVariablesCheckBox.setOpaque(false);
        mShowVariablesCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
        mShowSmartPathDebugCheckBox = new JCheckBox("Show Smart Path Calculation", false);
        mShowSmartPathDebugCheckBox.setOpaque(false);
        mShowSmartPathDebugCheckBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });

        // Do the Layout - pack all stuff into little small cute boxes
        JPanel fontAndSize = new JPanel();

        fontAndSize.setOpaque(false);
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

        drawOptions.setOpaque(false);
        drawOptions.setLayout(new BoxLayout(drawOptions, BoxLayout.Y_AXIS));
        drawOptions.add(Box.createRigidArea(new Dimension(1, 15)));
        drawOptions.add(mGridCheckBox);
        drawOptions.add(mShowNodeIDCheckBox);
        drawOptions.add(mShowVariablesCheckBox);
        drawOptions.add(mShowSmartPathDebugCheckBox);

        JPanel activityOptions = new JPanel();

        activityOptions.setOpaque(false);
        activityOptions.setLayout(new BoxLayout(activityOptions, BoxLayout.Y_AXIS));
        activityOptions.add(mVisualizationCheckBox);
        activityOptions.add(mVisualizationTraceCheckBox);

        JPanel graphicOptions = new JPanel();

        graphicOptions.setOpaque(false);
        graphicOptions.setLayout(new BoxLayout(graphicOptions, BoxLayout.X_AXIS));
        graphicOptions.add(Box.createRigidArea(new Dimension(5, 0)));
        graphicOptions.add(drawOptions);
        graphicOptions.add(Box.createHorizontalGlue());
        graphicOptions.add(activityOptions);
        graphicOptions.add(Box.createRigidArea(new Dimension(5, 0)));
        mGraphicsPanel = new JPanel();
        mGraphicsPanel.setBackground(Color.white);
        mGraphicsPanel.setLayout(new BoxLayout(mGraphicsPanel, BoxLayout.Y_AXIS));
        mGraphicsPanel.setBorder(BorderFactory.createTitledBorder(" Visual Appearance "));
        mGraphicsPanel.add(Box.createRigidArea(new Dimension(5, 20)));
        mGraphicsPanel.add(fontAndSize);
        mGraphicsPanel.add(graphicOptions);
        mGraphicsPanel.add(Box.createRigidArea(new Dimension(5, 20)));

    }

    private void initScriptPanel() {

        mScriptFontTypeLabel = new JLabel("Font Type:");

        GraphicsEnvironment g = GraphicsEnvironment.getLocalGraphicsEnvironment();
        String[] allFonts = g.getAvailableFontFamilyNames();
        ArrayList<String> fonts = new ArrayList<>();

        for (String font : allFonts) {
            if (font.contains("Mono")) {
                fonts.add(font);
            }
        }

//      JPanel controlPanel = new JPanel();
        mScriptFontComboBox = new JComboBox(fonts.toArray());
        mScriptFontComboBox.setOpaque(false);
        mScriptFontComboBox.setSelectedItem(mEditorConfig.sSCRIPT_FONT_TYPE);
        mScriptFontComboBox.addActionListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
        mScriptFontSizeLabel = new JLabel("Font Size:");
        mScriptFontSizeSpinner = new JSpinner(new SpinnerNumberModel(mEditorConfig.sSCRIPT_FONT_SIZE, 8, 16, 1));

        JPanel fontAndSize = new JPanel();
        fontAndSize.setOpaque(false);
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
        mScriptPanel.setBackground(Color.white);
        mScriptPanel.setLayout(new BoxLayout(mScriptPanel, BoxLayout.Y_AXIS));
        mScriptPanel.setBorder(BorderFactory.createTitledBorder(" Script Options "));
        mScriptPanel.add(Box.createRigidArea(new Dimension(5, 20)));
        mScriptPanel.add(fontAndSize);
        mScriptPanel.add(Box.createRigidArea(new Dimension(5, 20)));
    }


    private void saveEditorConfig(boolean dispose) {

        mEditorConfig.setProperty(
                "node_width", Integer.toString(((SpinnerNumberModel) mNodeSizeSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty(
                "node_height", Integer.toString(((SpinnerNumberModel) mNodeSizeSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty(
                "grid_x", Integer.toString(((SpinnerNumberModel) mGridScaleSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty(
                "grid_y", Integer.toString(((SpinnerNumberModel) mGridScaleSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty(
                "workspace_fontsize",
                Integer.toString(((SpinnerNumberModel) mWorkspaceFontSizeSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty("grid", Boolean.toString(mGridCheckBox.isSelected()));
        mEditorConfig.setProperty("visualization", Boolean.toString(mVisualizationCheckBox.isSelected()));
        mEditorConfig.setProperty("visualizationtrace", Boolean.toString(mVisualizationTraceCheckBox.isSelected()));
        mEditorConfig.setProperty("shownodeid", Boolean.toString(mShowNodeIDCheckBox.isSelected()));
        mEditorConfig.setProperty("showvariables", Boolean.toString(mShowVariablesCheckBox.isSelected()));
        mEditorConfig.setProperty("showsmartpathcalculations",
                Boolean.toString(mShowSmartPathDebugCheckBox.isSelected()));
        mEditorConfig.setProperty(
                "scriptfonsize",
                Integer.toString(((SpinnerNumberModel) mScriptFontSizeSpinner.getModel()).getNumber().intValue()));
        mEditorConfig.setProperty("scriptfonttype", mScriptFontComboBox.getSelectedItem().toString());

        mEditorConfig.save(mEditor.getSelectedProjectEditor().getEditorProject().getProjectFile());

        EventDispatcher.getInstance().convey(new ProjectChangedEvent(this));
//        EditorInstance.getInstance().save();
        if (dispose) {
            dispose();
        }
    }

    private void initEditorConfig() {

        for (Object keyObj : mEditorConfig.getKeySet()) {
            String key = (String) keyObj;

            if (key.startsWith("recentfile")) {
                ((DefaultListModel) mRecentFileList.getModel()).addElement(mEditorConfig.getProperty(key));
            } else if (key.equals("node_width")) {
                ((SpinnerNumberModel) mNodeSizeSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("node_height")) {
                ((SpinnerNumberModel) mNodeSizeSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("grid_x")) {
                ((SpinnerNumberModel) mGridScaleSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("grid_y")) {
                ((SpinnerNumberModel) mGridScaleSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("grid")) {
                mGridCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("scriptfontype")) {
                mVisualizationCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("scriptfonsize")) {
                ((SpinnerNumberModel) mScriptFontSizeSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("scriptfonttype")) {
                mScriptFontComboBox.setSelectedItem(mEditorConfig.getProperty(key));
            } else if (key.equals("visualizationtrace")) {
                mVisualizationTraceCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("launchPlayer")) {
                //mLaunchDefaultPlayerCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("shownodeid")) {
                mShowNodeIDCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("showvariables")) {
                mShowVariablesCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("showsmartpathcalculations")) {
                mShowSmartPathDebugCheckBox.setSelected(Boolean.valueOf(mEditorConfig.getProperty(key)));
            } else if (key.equals("workspace_fontsize")) {
                ((SpinnerNumberModel) mWorkspaceFontSizeSpinner.getModel()).setValue(
                        Integer.valueOf(mEditorConfig.getProperty(key)));
            }

        } 
        // Add specific listeners
        mNodeSizeSpinner.addChangeListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });

        // Add specific listeners
        mScriptFontSizeSpinner.addChangeListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });

        // Add specific listeners
        mGridScaleSpinner.addChangeListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });

        // Add specific listeners
        mWorkspaceFontSizeSpinner.addChangeListener(e -> {
            saveEditorConfig(false);
            mEditor.refresh();
        });
    }
}
