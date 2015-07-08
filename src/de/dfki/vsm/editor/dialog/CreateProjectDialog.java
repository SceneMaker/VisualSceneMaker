package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.acticon.ActiconConfig;
import de.dfki.vsm.model.config.ConfigElement;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.gesticon.GesticonConfig;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.visicon.VisiconConfig;
import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.util.xml.XMLWriteError;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.MouseInputListener;

/**
 * @author Patrick Gebhard
 */
public class CreateProjectDialog extends JDialog {

    // panels
    private JPanel mMainPanel = null;
    private JPanel mConfigPanel = null;
    private JPanel mButtonsPanel = null;

    // buttons
    private OKButton mOkButton = null;
    private CancelButton mCancelButton = null;

    // text fields
    private JTextField mLocationTextField = null;
    private JTextField mNameTextField = null;
    private JTextField mSceneFlowTextField = null;
    private JLabel mSceneFlowMessage = new JLabel(" ");
    private JTextField mScenesTextField = null;
    private JLabel mSceneMessage = new JLabel(" ");
    private JTextField mGesticonTextField = null;
    private JTextField mVisiconTextField = null;
    private JTextField mDefaultScenePlayerTextField = null;
    private JTextField mDefaultScenePlayerConfigTextField = null;
    private ArrayList<JTextField> mScenePlayerTextFields = null;
    private ArrayList<JTextField> mScenePlayerConfigTextFields = null;

    // flags
    private boolean mLocationDialog = false;

    // logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // content representation
    private ArrayList<ConfigFeature> mProperties = new ArrayList<ConfigFeature>();
    private JButton mLocationButton;

    // Labels
    private JLabel errorMsg;
    private JLabel lblName;
    private JLabel lblPath;

    // files
    public File mProjectDir;
    public File mConfigFile;
    public File mSceneFlowFile;
    public File mScenesFile;
    public File mGesticonFile;
    public File mVisiconFile;
    public File mActiconFile;
    //private String mPreferencesFilePath;

    // essential strings
    public String mProjectName;
    private ConfigElement mProjectConfig;

    public CreateProjectDialog() {
        super(EditorInstance.getInstance(), "New Project", true);
        mProperties.add(new ConfigFeature("Feature", "project.basic.name", "project"));
        mProperties.add(new ConfigFeature("Feature", "project.data.scenes", "scenes.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.data.acticon", "acticon.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.data.visicon", "visicon.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.data.gesticon", "gesticon.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.data.sceneflow", "sceneflow.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.data.preferences", "preferences.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.dialogact.class", "de.dfki.vsm.runtime.dialogact.DummyDialogAct"));
        mProperties.add(new ConfigFeature("Feature", "project.dialogact.player",
                "de.dfki.vsm.runtime.player.DefaultDialogueActPlayer"));
        mProperties.add(new ConfigFeature("Feature", "project.player.config", "player.xml"));
        mProperties.add(new ConfigFeature("Feature", "project.player.class", "de.dfki.vsm.runtime.player.DefaultSceneGroupPlayer"));
        mProjectConfig = new ConfigElement("Feature", "ProjectConfig", mProperties);
        initComponents();
        setVisible(true);
    }

    private void initComponents() {

        // create contentfields and set inital content
        Dimension tSize = new Dimension(250, 30);
        Dimension labelSize = new Dimension(100, 30);
        Dimension buttonSize = new Dimension(125, 30);

        setBackground(Color.white);
        mNameTextField = new JTextField(mProjectConfig.getProperty("project.basic.name"));
        mNameTextField.setMinimumSize(tSize);
        mNameTextField.setPreferredSize(tSize);
        mNameTextField.addMouseListener(new MouseInputListener() {
            @Override
            public void mouseClicked(MouseEvent me) {
                mNameTextField.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            }

            @Override
            public void mousePressed(MouseEvent me) {
            }

            @Override
            public void mouseReleased(MouseEvent me) {
            }

            @Override
            public void mouseEntered(MouseEvent me) {
            }

            @Override
            public void mouseExited(MouseEvent me) {
            }

            @Override
            public void mouseDragged(MouseEvent me) {
            }

            @Override
            public void mouseMoved(MouseEvent me) {
            }
        });
        mSceneFlowTextField = new JTextField(mProjectConfig.getProperty("project.data.sceneflow"));
        mSceneFlowTextField.setMinimumSize(tSize);
        mSceneFlowTextField.setPreferredSize(tSize);
        mSceneFlowTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(EditorInstance.getInstance(), "Select Sceneflow");

                if (file.getSelectedFile() != null) {
                    mSceneFlowTextField.setText(file.getSelectedFile().getPath());
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });
        mScenesTextField = new JTextField(mProjectConfig.getProperty("project.data.scenes"));
        mScenesTextField.setMinimumSize(tSize);
        mScenesTextField.setPreferredSize(tSize);
        mScenesTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(EditorInstance.getInstance(), "Select Scenes");

                if (file.getSelectedFile() != null) {
                    mScenesTextField.setText(file.getSelectedFile().getPath());
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });
        mGesticonTextField = new JTextField(mProjectConfig.getProperty("project.data.gesticon"));
        mGesticonTextField.setMinimumSize(tSize);
        mGesticonTextField.setPreferredSize(tSize);
        mGesticonTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(EditorInstance.getInstance(), "Select Gesticon");

                if (file.getSelectedFile() != null) {
                    mGesticonTextField.setText(file.getSelectedFile().getPath());
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });
        mVisiconTextField = new JTextField(mProjectConfig.getProperty("project.data.visicon"));
        mVisiconTextField.setMinimumSize(tSize);
        mVisiconTextField.setPreferredSize(tSize);
        mVisiconTextField.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                JFileChooser file = new JFileChooser(de.dfki.vsm.editor.util.Preferences.sUSER_HOME);

                file.setFileSelectionMode(JFileChooser.FILES_ONLY);
                file.showDialog(EditorInstance.getInstance(), "Select Visicon");

                if (file.getSelectedFile() != null) {
                    mVisiconTextField.setText(file.getSelectedFile().getPath());
                }
            }

            @Override
            public void mousePressed(MouseEvent e) {
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });
        mDefaultScenePlayerTextField = new JTextField(mProjectConfig.getProperty("project.player.class"));
        mDefaultScenePlayerTextField.setEditable(false);
        mDefaultScenePlayerConfigTextField = new JTextField(mProjectConfig.getProperty("project.player.config"));
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
                    file.showDialog(EditorInstance.getInstance(), "Select SceneMaker Project Path");

                    if (file.getSelectedFile() != null) {
                        mLocationTextField.setText(file.getSelectedFile().getPath());
                    }

                    locationDialogShown();
                    checkFileConsistency();
                }
            }

            @Override
            public void focusLost(FocusEvent e) {
            }
        });
        mLocationButton = new JButton(ResourceLoader.loadImageIcon("/res/img/search_icon.png"));
        mLocationButton.setOpaque(false);
        mLocationButton.setFocusable(false);
        mLocationButton.setBorder(null);
        mLocationButton.setContentAreaFilled(false);
        mLocationButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/search_icon_blue.png"));
        mLocationButton.setToolTipText("Search Path...");
        mLocationButton.addMouseListener(new MouseListener() {
            @Override
            public void mouseClicked(MouseEvent e) {
                mLocationTextField.setBorder(BorderFactory.createLineBorder(Color.GRAY));
            }

            @Override
            public void mousePressed(MouseEvent e) {
                locationDialogShown();

                JFileChooser file = new JFileChooser(mLocationTextField.getText().isEmpty()
                        ? de.dfki.vsm.editor.util.Preferences.sUSER_HOME
                        : mLocationTextField.getText());

                file.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                file.showDialog(EditorInstance.getInstance(), "Select SceneMaker Project Path");

                if (file.getSelectedFile() != null) {
                    mLocationTextField.setText(file.getSelectedFile().getPath());
                }

                checkFileConsistency();
            }

            @Override
            public void mouseReleased(MouseEvent e) {
            }

            @Override
            public void mouseEntered(MouseEvent e) {
            }

            @Override
            public void mouseExited(MouseEvent e) {
            }
        });

        // create buttons
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });

//      mOkButton.addActionListener(new ActionListener() {
//          @Override
//          public void actionPerformed(ActionEvent evt) {
//              okActionPerformed();
//          }
//      });
        // mOkButton.setSelected(true);
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });

//      mCancelButton.addActionListener(new ActionListener() {
//          @Override
//          public void actionPerformed(ActionEvent evt) {
//              cancelActionPerformed();
//          }
//      });
        // create config panel
        mConfigPanel = new JPanel();
        mConfigPanel.setOpaque(false);
        mConfigPanel.setLayout(new BoxLayout(mConfigPanel, BoxLayout.Y_AXIS));

        JPanel locationPanel = new JPanel();

        locationPanel.setOpaque(false);
        locationPanel.setLayout(new BoxLayout(locationPanel, BoxLayout.X_AXIS));
        locationPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        lblPath = new JLabel("Path :");
        lblPath.setPreferredSize(labelSize);
        locationPanel.add(lblPath);
        locationPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        locationPanel.add(mLocationTextField);
        locationPanel.add(Box.createRigidArea(new Dimension(22, 0)));
        locationPanel.add(mLocationButton);
        locationPanel.add(Box.createRigidArea(new Dimension(5, 1)));

        JPanel namePanel = new JPanel();

        namePanel.setOpaque(false);
        namePanel.setLayout(new BoxLayout(namePanel, BoxLayout.X_AXIS));
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        lblName = new JLabel("Name :");
        lblName.setPreferredSize(labelSize);
        namePanel.add(lblName);
        namePanel.add(Box.createRigidArea(new Dimension(5, 0)));
        namePanel.add(mNameTextField);
        namePanel.add(Box.createRigidArea(new Dimension(50, 0)));

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
        mConfigPanel.setBorder(BorderFactory.createEmptyBorder(20, 20, 20, 20));
        mConfigPanel.add(Box.createRigidArea(new Dimension(0, 30)));
        mConfigPanel.add(locationPanel);
        mConfigPanel.add(Box.createRigidArea(new Dimension(0, 10)));
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        mConfigPanel.add(errorMsg);

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
        // compose panels
        mButtonsPanel = new JPanel();
        mButtonsPanel.setOpaque(false);
        mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
        mButtonsPanel.setBorder(BorderFactory.createEmptyBorder(0, 20, 0, 40));
        mButtonsPanel.add(Box.createHorizontalGlue());
        mButtonsPanel.add(mCancelButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(50, 1)));
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(Box.createRigidArea(new Dimension(30, 0)));
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
        mMainPanel.setOpaque(false);
        mMainPanel.add(Box.createRigidArea(new Dimension(0, 15)));
        mMainPanel.add(mConfigPanel);
        mMainPanel.add(Box.createVerticalGlue());
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(Box.createRigidArea(new Dimension(0, 20)));
        add(mMainPanel);
//        getContentPane().setBackground(Color.white);
        setResizable(false);
        pack();

        // setSize(400, 300);
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

            if (mSceneFlowTextField.getText().equalsIgnoreCase(mProjectConfig.getProperty("project.data.sceneflow"))) {
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
                        + mProjectConfig.getProperty("project.data.sceneflow"));
                mLogger.message("copying file " + source + " to " + mSceneFlowFile);
                copyFile(source, mSceneFlowFile);
            }

            if (mScenesTextField.getText().equalsIgnoreCase(mProjectConfig.getProperty("project.data.scenes"))) {
                mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mScenesTextField.getText());

                if (!mScenesFile.exists()) {
                    mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                            + mScenesTextField.getText());
                    mLogger.message("creating file " + mScenesFile);

                    SceneScript dummyScene = new SceneScript();
                    IndentWriter out = new IndentWriter(mScenesFile);

                    dummyScene.writeXML(out);
                    out.close();
                } else {
                    mLogger.message("using existing file " + mScenesFile);
                }
            } else {
                File source = new File(mScenesTextField.getText());

                mScenesFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mProjectConfig.getProperty("project.scenes"));
                mLogger.message("copying file " + source + " to " + mScenesFile);
                copyFile(source, mScenesFile);
            }

            if (mGesticonTextField.getText().equalsIgnoreCase(mProjectConfig.getProperty("project.data.gesticon"))) {
                mGesticonFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mGesticonTextField.getText());
                mLogger.message("creating file " + mGesticonFile);

                GesticonConfig dummyGesticon = new GesticonConfig();
                IndentWriter out = new IndentWriter(mGesticonFile);

                dummyGesticon.writeXML(out);
                out.close();

                // mGesticonFile.createNewFile();
            } else {
                File source = new File(mGesticonTextField.getText());

                mGesticonFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mProjectConfig.getProperty("project.gesticon"));
                mLogger.message("copying file " + source + " to " + mGesticonFile);
                copyFile(source, mGesticonFile);
            }

            if (mVisiconTextField.getText().equalsIgnoreCase(mProjectConfig.getProperty("project.data.visicon"))) {
                mVisiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mVisiconTextField.getText());
                mLogger.message("creating file " + mVisiconFile);

                VisiconConfig dummyVisicon = new VisiconConfig();
                IndentWriter out = new IndentWriter(mVisiconFile);

                dummyVisicon.writeXML(out);
                out.close();

                // mVisiconFile.createNewFile();
            } else {
                File source = new File(mVisiconTextField.getText());

                mVisiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                        + mProjectConfig.getProperty("project.data.visicon"));
                mLogger.message("copying file " + source + " to " + mVisiconFile);
                copyFile(source, mVisiconFile);
            }

            // TODO: add acticon file
            mActiconFile = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                    + mProjectConfig.getProperty("project.data.acticon"));
            mLogger.message("creating file " + mActiconFile);

            ActiconConfig dummyActicon = new ActiconConfig();
            IndentWriter out = new IndentWriter(mActiconFile);

            dummyActicon.writeXML(out);
            out.close();

            // create scene player config file
            File scenePlayerConfig = new File(mLocationTextField.getText() + System.getProperty("file.separator")
                    + mDefaultScenePlayerConfigTextField.getText());

            mLogger.message("creating file " + scenePlayerConfig);

            ConfigElement dummyPlayerConfig = new ConfigElement("Player", "Feature");
            IndentWriter fileOut = new IndentWriter(scenePlayerConfig);

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
           // mPreferencesFilePath = (mLocationTextField.getText() + System.getProperty("file.separator")
           //         + mProjectConfig.getProperty("project.data.preferences"));
           // mLogger.message("creating file " + mPreferencesFilePath);

            EditorConfig dummyPreferences = new EditorConfig();

            //dummyPreferences.load("");
            //dummyPreferences.save(mPreferencesFilePath);

            // build config file
            mProperties.set(0, new ConfigFeature("Feature", "project.basic.name", mNameTextField.getText()));
            mConfigFile = new File(mLocationTextField.getText() + System.getProperty("file.separator") + "config.xml");
            mLogger.message("creating file " + mConfigFile);
            XMLUtilities.writeToXMLFile(mProjectConfig, mConfigFile);
        } catch (XMLWriteError | IOException e) {
            e.printStackTrace(System.out);
        }
    }

    private boolean validateValues() {
        if (mNameTextField.getText().length() == 0) {
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);

            return false;
        }

        if (mLocationTextField.getText().length() == 0) {
            mLocationTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);

            return false;
        }

        errorMsg.setForeground(Color.white);

        // TODO!
        // check exisiting files
        return true;
    }

    public static void copyFile(File input, File output) {
        try {
            FileReader in = new FileReader(input);
            FileWriter out = new FileWriter(output);
            int c;

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
