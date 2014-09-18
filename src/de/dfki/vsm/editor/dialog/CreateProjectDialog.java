package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.acticon.ActiconObject;
import de.dfki.vsm.model.configs.ConfigEntry;
import de.dfki.vsm.model.configs.PlayerConfig;
import de.dfki.vsm.model.configs.ProjectConfig;
import de.dfki.vsm.model.configs.ProjectPreferences;
import de.dfki.vsm.model.gesticon.GesticonObject;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.script.SceneScript;
import de.dfki.vsm.model.visicon.VisiconObject;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseTools;
import de.dfki.vsm.util.xml.XMLWriteError;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import java.util.ArrayList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 * @author Patrick Gebhard
 */
public class CreateProjectDialog extends JDialog {
    
        // panels
    private JPanel mMainPanel    = null;
    private JPanel mConfigPanel  = null;
    private JPanel mButtonsPanel = null;

    // buttons
    private JButton mOkButton     = null;
    private JButton mCancelButton = null;

    // text fields
    private JTextField            mLocationTextField                 = null;
    private JTextField            mNameTextField                     = null;
    private JTextField            mSceneFlowTextField                = null;
    private JLabel                mSceneFlowMessage                  = new JLabel(" ");
    private JTextField            mScenesTextField                   = null;
    private JLabel                mSceneMessage                      = new JLabel(" ");
    private JTextField            mGesticonTextField                 = null;
    private JTextField            mVisiconTextField                  = null;
    private JTextField            mDefaultScenePlayerTextField       = null;
    private JTextField            mDefaultScenePlayerConfigTextField = null;
    private ArrayList<JTextField> mScenePlayerTextFields             = null;
    private ArrayList<JTextField> mScenePlayerConfigTextFields       = null;

    // flags
    private boolean mLocationDialog = false;

    // logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // content representation
    private ArrayList<ConfigEntry> mProperties = new ArrayList<ConfigEntry>();

    // files
    public File    mProjectDir;
    public File    mConfigFile;
    public File    mSceneFlowFile;
    public File    mScenesFile;
    public File    mGesticonFile;
    public File    mVisiconFile;
    public File    mActiconFile;
    private String mPreferencesFilePath;

    // essential strings
    public String         mProjectName;
    private ProjectConfig mProjectConfig;

    public CreateProjectDialog() {
        super(Editor.getInstance(), "New Project", true);
        mProperties.add(new ConfigEntry("project.basic.name", "project"));
        mProperties.add(new ConfigEntry("project.data.scenes", "scenes.xml"));
        mProperties.add(new ConfigEntry("project.data.acticon", "acticon.xml"));
        mProperties.add(new ConfigEntry("project.data.visicon", "visicon.xml"));
        mProperties.add(new ConfigEntry("project.data.gesticon", "gesticon.xml"));
        mProperties.add(new ConfigEntry("project.data.sceneflow", "sceneflow.xml"));
        mProperties.add(new ConfigEntry("project.data.preferences", "preferences.xml"));
        
        mProperties.add(new ConfigEntry("project.dialogact.class", "de.dfki.vsm.runtime.dialogact.DummyDialogAct"));
        mProperties.add(new ConfigEntry("project.dialogact.player", "de.dfki.vsm.runtime.player.DefaultDialogueActPlayer"));
      
        
        mProperties.add(new ConfigEntry("project.player.config", "player.xml"));
        mProperties.add(new ConfigEntry("project.player.class", "de.dfki.vsm.runtime.player.DefaultSceneGroupPlayer"));
        
       
        
        mProjectConfig = new ProjectConfig(mProperties);
        initComponents();
        setVisible(true);
    }

    private void initComponents() {

        // create contentfields and set inital content
        Dimension tSize = new Dimension(150, 30);

        mNameTextField = new JTextField(mProjectConfig.property("project.basic.name"));
        mNameTextField.setMinimumSize(tSize);
        mNameTextField.setPreferredSize(tSize);
        mSceneFlowTextField = new JTextField(mProjectConfig.property("project.data.sceneflow"));
        mSceneFlowTextField.setMinimumSize(tSize);
        mSceneFlowTextField.setPreferredSize(tSize);
        mSceneFlowTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(Editor.getInstance(), "Select Sceneflow");

                if (file.getSelectedFile() != null) {
                    mSceneFlowTextField.setText(file.getSelectedFile().getPath());
                }
            }
            @Override
            public void mousePressed(MouseEvent e) {}
            @Override
            public void mouseReleased(MouseEvent e) {}
            @Override
            public void mouseEntered(MouseEvent e) {}
            @Override
            public void mouseExited(MouseEvent e) {}
        });
        mScenesTextField = new JTextField(mProjectConfig.property("project.data.scenes"));
        mScenesTextField.setMinimumSize(tSize);
        mScenesTextField.setPreferredSize(tSize);
        mScenesTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(Editor.getInstance(), "Select Scenes");

                if (file.getSelectedFile() != null) {
                    mScenesTextField.setText(file.getSelectedFile().getPath());
                }
            }
            @Override
            public void mousePressed(MouseEvent e) {}
            @Override
            public void mouseReleased(MouseEvent e) {}
            @Override
            public void mouseEntered(MouseEvent e) {}
            @Override
            public void mouseExited(MouseEvent e) {}
        });
        mGesticonTextField = new JTextField(mProjectConfig.property("project.data.gesticon"));
        mGesticonTextField.setMinimumSize(tSize);
        mGesticonTextField.setPreferredSize(tSize);
        mGesticonTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(Editor.getInstance(), "Select Gesticon");

                if (file.getSelectedFile() != null) {
                    mGesticonTextField.setText(file.getSelectedFile().getPath());
                }
            }
            @Override
            public void mousePressed(MouseEvent e) {}
            @Override
            public void mouseReleased(MouseEvent e) {}
            @Override
            public void mouseEntered(MouseEvent e) {}
            @Override
            public void mouseExited(MouseEvent e) {}
        });
        mVisiconTextField = new JTextField(mProjectConfig.property("project.data.visicon"));
        mVisiconTextField.setMinimumSize(tSize);
        mVisiconTextField.setPreferredSize(tSize);
        mVisiconTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(Editor.getInstance(), "Select Visicon");

                if (file.getSelectedFile() != null) {
                    mVisiconTextField.setText(file.getSelectedFile().getPath());
                }
            }
            @Override
            public void mousePressed(MouseEvent e) {}
            @Override
            public void mouseReleased(MouseEvent e) {}
            @Override
            public void mouseEntered(MouseEvent e) {}
            @Override
            public void mouseExited(MouseEvent e) {}
        });
        mDefaultScenePlayerTextField = new JTextField(mProjectConfig.property("project.player.class"));
        mDefaultScenePlayerTextField.setEditable(false);
        mDefaultScenePlayerConfigTextField = new JTextField(mProjectConfig.property("project.player.config"));
        mDefaultScenePlayerConfigTextField.setEditable(false);
        mLocationTextField = new JTextField();
        mLocationTextField.setMinimumSize(tSize);
        mLocationTextField.setPreferredSize(tSize);
        mLocationTextField.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                if (!getLocationDialogShownState()) {
                    JFileChooser file = new JFileChooser(mLocationTextField.getText().isEmpty()
                            ? de.dfki.vsm.editor.util.Preferences.sUSER_HOME
                            : mLocationTextField.getText());

                    file.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                    file.showDialog(Editor.getInstance(), "Select SceneMaker Project Path");

                    if (file.getSelectedFile() != null) {
                        mLocationTextField.setText(file.getSelectedFile().getPath());
                    }

                    locationDialogShown();
                    checkFileConsistency();
                }
            }
            @Override
            public void focusLost(FocusEvent e) {}
        });
        mLocationTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {}
            @Override
            public void mousePressed(MouseEvent e) {
                locationDialogShown();

                JFileChooser file = new JFileChooser(mLocationTextField.getText().isEmpty()
                        ? de.dfki.vsm.editor.util.Preferences.sUSER_HOME
                        : mLocationTextField.getText());

                file.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                file.showDialog(Editor.getInstance(), "Select SceneMaker Project Path");

                if (file.getSelectedFile() != null) {
                    mLocationTextField.setText(file.getSelectedFile().getPath());
                }

                checkFileConsistency();
            }
            @Override
            public void mouseReleased(MouseEvent e) {}
            @Override
            public void mouseEntered(MouseEvent e) {}
            @Override
            public void mouseExited(MouseEvent e) {}
        });

        // create buttons
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                okActionPerformed();
            }
        });
        mOkButton.setSelected(true);
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                cancelActionPerformed();
            }
        });

        // create config panel
        mConfigPanel = new JPanel();
        mConfigPanel.setOpaque(false);
        mConfigPanel.setLayout(new BoxLayout(mConfigPanel, BoxLayout.Y_AXIS));

        JPanel locationPanel = new JPanel();

        locationPanel.setOpaque(false);
        locationPanel.setLayout(new BoxLayout(locationPanel, BoxLayout.X_AXIS));
        locationPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        locationPanel.add(new JLabel("Path"));
        locationPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        locationPanel.add(mLocationTextField);
        locationPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        JPanel namePanel = new JPanel();

        namePanel.setOpaque(false);
        namePanel.setLayout(new BoxLayout(namePanel, BoxLayout.X_AXIS));
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        namePanel.add(new JLabel("Name"));
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        namePanel.add(mNameTextField);
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));

//      JPanel sceneFlowPanel = new JPanel();
//      sceneFlowPanel.setLayout(new BoxLayout(sceneFlowPanel, BoxLayout.X_AXIS));
//      sceneFlowPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      sceneFlowPanel.add(new JLabel("Path to SceneFlow"));
//      sceneFlowPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      sceneFlowPanel.add(mSceneFlowTextField);
//      sceneFlowPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//
//      JPanel scenePanel = new JPanel();
//      scenePanel.setLayout(new BoxLayout(scenePanel, BoxLayout.X_AXIS));
//      scenePanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      scenePanel.add(new JLabel("Path to Scenes"));
//      scenePanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      scenePanel.add(mScenesTextField);
//      scenePanel.add(Box.createRigidArea(new Dimension(5, 0)));
//
//      JPanel gesticonPanel = new JPanel();
//      gesticonPanel.setLayout(new BoxLayout(gesticonPanel, BoxLayout.X_AXIS));
//      gesticonPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      gesticonPanel.add(new JLabel("Path to Gesticon"));
//      gesticonPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      gesticonPanel.add(mGesticonTextField);
//      gesticonPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//
//      JPanel visiconPanel = new JPanel();
//      visiconPanel.setLayout(new BoxLayout(visiconPanel, BoxLayout.X_AXIS));
//      visiconPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      visiconPanel.add(new JLabel("Path to Visicon"));
//      visiconPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      visiconPanel.add(mVisiconTextField);
//      visiconPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//
//      JPanel defaultScenePlayerPanel = new JPanel();
//      defaultScenePlayerPanel.setLayout(new BoxLayout(defaultScenePlayerPanel, BoxLayout.X_AXIS));
//      defaultScenePlayerPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      defaultScenePlayerPanel.add(new JLabel("ScenePlayer"));
//      defaultScenePlayerPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//      defaultScenePlayerPanel.add(mDefaultScenePlayerTextField);
//      defaultScenePlayerPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        JPanel defaultScenePlayerConfigPanel = new JPanel();

        defaultScenePlayerConfigPanel.setLayout(new BoxLayout(defaultScenePlayerConfigPanel, BoxLayout.X_AXIS));
        defaultScenePlayerConfigPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        defaultScenePlayerConfigPanel.add(new JLabel("Sceneplayer Config"));
        defaultScenePlayerConfigPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        defaultScenePlayerConfigPanel.add(mDefaultScenePlayerConfigTextField);
        defaultScenePlayerConfigPanel.add(Box.createRigidArea(new Dimension(5, 0)));

        // compose config panel
        mConfigPanel.add(namePanel);
        mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
        mConfigPanel.add(locationPanel);
        mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));

//      mConfigPanel.add(sceneFlowPanel);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 5)));
//      mConfigPanel.add(mSceneFlowMessage);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
//      mConfigPanel.add(scenePanel);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 5)));
//      mConfigPanel.add(mSceneMessage);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
//      mConfigPanel.add(gesticonPanel);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
//      mConfigPanel.add(visiconPanel);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
//      mConfigPanel.add(defaultScenePlayerPanel);
//      mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
//      mConfigPanel.add(defaultScenePlayerConfigPanel);
        mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));

        // compose panels
        mButtonsPanel = new JPanel();
        mButtonsPanel.setOpaque(false);
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mCancelButton);
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mMainPanel = new JPanel();

//      {  
//          public void paintComponent(Graphics g) {  
//              final Graphics2D g2d = (Graphics2D) g;
//              g2d.setRenderingHint(
//                  RenderingHints.KEY_ANTIALIASING,
//                  RenderingHints.VALUE_ANTIALIAS_ON);
//              g2d.setRenderingHint(
//                  RenderingHints.KEY_TEXT_ANTIALIASING,
//                  RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
//              g2d.setRenderingHint(
//                  RenderingHints.KEY_RENDERING,
//                  RenderingHints.VALUE_RENDER_SPEED);
//              //            g2d.setRenderingHint(
//                  //                    RenderingHints.KEY_FRACTIONALMETRICS,
//                  //                    RenderingHints.VALUE_FRACTIONALMETRICS_ON);
//              g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
//                  RenderingHints.VALUE_INTERPOLATION_BILINEAR);
//              Image img = Toolkit.getDefaultToolkit().getImage(  
//                  DefaultEditor.class.getResource("/res/img/docicon.png"));
//              g2d.setComposite(AlphaComposite.getInstance( AlphaComposite.SRC_OVER, 0.25f));
//              g2d.drawImage(img, -10, -20, 180, 180, this);
//              g2d.setComposite(AlphaComposite.getInstance( AlphaComposite.SRC_OVER, 1.0f));
//          }  
//      };
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(Box.createRigidArea(new Dimension(0, 15)));
        mMainPanel.add(mConfigPanel);
        mMainPanel.add(Box.createVerticalGlue());
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(Box.createRigidArea(new Dimension(0, 5)));
        add(mMainPanel);
        setResizable(false);
        pack();

        // setSize(550, 425);
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
    }
    
    protected void locationDialogShown() {
        mLocationDialog = true;
    }

    protected boolean getLocationDialogShownState() {
        return mLocationDialog;
    }

    protected void okActionPerformed() {
        if (validateValues()) {
            createProject();
            dispose();
        }
    }

    protected void checkFileConsistency() {
        File sceneFlowFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                      + mSceneFlowTextField.getText());

        if (sceneFlowFile.exists()) {
            mSceneFlowMessage.setText("Using existing file at given location!");
        } else {
            mSceneFlowMessage.setText(" ");
        }

        File scenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                   + mScenesTextField.getText());

        if (scenesFile.exists()) {
            mSceneMessage.setText("Using existing file at given location!");
        } else {
            mSceneMessage.setText(" ");
        }

        repaint();
    }

    protected void cancelActionPerformed() {
        dispose();
    }

    private void createProject() {
        try {

            // check if target dir exisits
            mProjectDir = new File(mLocationTextField.getText());

            if (!mProjectDir.exists()) {
                mLogger.message("creating dir " + mProjectDir.toString());
                mProjectDir.mkdir();
            }

            if (mSceneFlowTextField.getText().equalsIgnoreCase(mProjectConfig.property("project.data.sceneflow"))) {
                mSceneFlowFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                          + mSceneFlowTextField.getText());

                if (!mSceneFlowFile.exists()) {
                    mLogger.message("creating file " + mSceneFlowFile);

                    SceneFlow dummySceneFlow = new SceneFlow();

                    dummySceneFlow.setNameAndId(mNameTextField.getText().trim());

                    IndentWriter out = new IndentWriter(mSceneFlowFile);

                    dummySceneFlow.writeXML(out);
                    out.close();
                } else {
                    mLogger.message("using existing file " + mSceneFlowFile);
                }
            } else {
                File source = new File(mSceneFlowTextField.getText());

                mSceneFlowFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                          + mProjectConfig.property("project.data.sceneflow"));
                mLogger.message("copying file " + source + " to " + mSceneFlowFile);
                copyFile(source, mSceneFlowFile);
            }

            if (mScenesTextField.getText().equalsIgnoreCase(mProjectConfig.property("project.data.scenes"))) {
                mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                       + mScenesTextField.getText());

                if (!mScenesFile.exists()) {
                    mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                           + mScenesTextField.getText());
                    mLogger.message("creating file " + mScenesFile);

                    SceneScript  dummyScene = new SceneScript();
                    IndentWriter out        = new IndentWriter(mScenesFile);

                    dummyScene.writeXML(out);
                    out.close();
                } else {
                    mLogger.message("using existing file " + mScenesFile);
                }
            } else {
                File source = new File(mScenesTextField.getText());

                mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                       + mProjectConfig.property("project.scenes"));
                mLogger.message("copying file " + source + " to " + mScenesFile);
                copyFile(source, mScenesFile);
            }

            if (mGesticonTextField.getText().equalsIgnoreCase(mProjectConfig.property("project.data.gesticon"))) {
                mGesticonFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                         + mGesticonTextField.getText());
                mLogger.message("creating file " + mGesticonFile);

                GesticonObject dummyGesticon = new GesticonObject();
                IndentWriter   out           = new IndentWriter(mGesticonFile);

                dummyGesticon.writeXML(out);
                out.close();

                // mGesticonFile.createNewFile();
            } else {
                File source = new File(mGesticonTextField.getText());

                mGesticonFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                         + mProjectConfig.property("project.gesticon"));
                mLogger.message("copying file " + source + " to " + mGesticonFile);
                copyFile(source, mGesticonFile);
            }

            if (mVisiconTextField.getText().equalsIgnoreCase(mProjectConfig.property("project.data.visicon"))) {
                mVisiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                        + mVisiconTextField.getText());
                mLogger.message("creating file " + mVisiconFile);

                VisiconObject dummyVisicon = new VisiconObject();
                IndentWriter  out          = new IndentWriter(mVisiconFile);

                dummyVisicon.writeXML(out);
                out.close();

                // mVisiconFile.createNewFile();
            } else {
                File source = new File(mVisiconTextField.getText());

                mVisiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                        + mProjectConfig.property("project.data.visicon"));
                mLogger.message("copying file " + source + " to " + mVisiconFile);
                copyFile(source, mVisiconFile);
            }

            // TODO: add acticon file
            mActiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                    + mProjectConfig.property("project.data.acticon"));
            mLogger.message("creating file " + mActiconFile);

            ActiconObject dummyActicon = new ActiconObject();
            IndentWriter  out          = new IndentWriter(mActiconFile);

            dummyActicon.writeXML(out);
            out.close();

            // create scene player config file
            File scenePlayerConfig = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                                              + mDefaultScenePlayerConfigTextField.getText());

            mLogger.message("creating file " + scenePlayerConfig);

            PlayerConfig dummyPlayerConfig = new PlayerConfig();
            IndentWriter fileOut           = new IndentWriter(scenePlayerConfig);

            dummyPlayerConfig.writeXML(fileOut);
            fileOut.close();

            /*
             * File scenePlayerConfig = new File(mLocationTextField.getText() + System.getProperty("file.separator") + mDefaultScenePlayerConfigTextField.getText());
             * mLogger.message("creating file " + scenePlayerConfig);
             * FileOutputStream fileOut = new FileOutputStream(scenePlayerConfig);
             * Properties dspProp = new Properties();
             * dspProp.setProperty("language.supported", "de");
             * dspProp.setProperty("language.default", "de");
             * dspProp.setProperty("character.ids", "A,B");
             * dspProp.setProperty("character.defaultId", "A");
             * dspProp.store(fileOut, "Default Scene Player Configuration");
             * fileOut.close();
             */

            // Create Preferences File
            mPreferencesFilePath = (mLocationTextField.getText() + System.getProperty("file.separator")
                                    + mProjectConfig.property("project.data.preferences"));
            mLogger.message("creating file " + mPreferencesFilePath);

            ProjectPreferences dummyPreferences = new ProjectPreferences();

            dummyPreferences.load("");
            dummyPreferences.save(mPreferencesFilePath);

            // build config file
            mProperties.set(0, new ConfigEntry("project.basic.name", mNameTextField.getText()));
            mConfigFile = new File(mLocationTextField.getText() + System.getProperty("file.separator") + "config.xml");
            mLogger.message("creating file " + mConfigFile);
            XMLParseTools.writeToXMLFile(mProjectConfig, mConfigFile);
        } catch (XMLWriteError | IOException e) {
            e.printStackTrace(System.out);
        }
    }

    private boolean validateValues() {
        if (mNameTextField.getText().length() == 0) {
            return false;
        }

        if (mLocationTextField.getText().length() == 0) {
            return false;
        }

        // TODO!
        // check exisiting files
        return true;
    }

    public static void copyFile(File input, File output) {
        try {
            FileReader in  = new FileReader(input);
            FileWriter out = new FileWriter(output);
            int        c;

            while ((c = in.read()) != -1) {
                out.write(c);
            }

            in.close();
            out.close();
        } catch (IOException e) {
            e.printStackTrace(System.out);
        }
    }
}

