package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.AboutDialog;
import de.dfki.vsm.editor.dialog.CreateProjectDialog;
import de.dfki.vsm.editor.dialog.ErrorDialog;
import de.dfki.vsm.editor.dialog.MonitorDialog;
import de.dfki.vsm.editor.dialog.OptionsDialog;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.event.AbortEvent;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.UIManager;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public final class EditorInstance extends JFrame implements EventListener {

    // The singelton editor instance
    private static EditorInstance sInstance = null;

    // The singelton runtime instance 
    private final RunTimeInstance mRunTime = RunTimeInstance.getInstance();
    // The singelton logger instance   
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton event multicaster
    private final EventCaster mEventCaster = EventCaster.getInstance();
    // The editor's observable component 
    private final Observable mObservable = new Observable();

    // The observable class of the editor
    private final class Observable extends java.util.Observable {

        public final void update(final Object object) {
            setChanged();
            notifyObservers(object);
        }
    }

    // The editor's GUI components
    private final MenuBar mMenuBar;
    private final ProjectEditorList mProjectEditorList;
    private final WelcomePanel mWelcomePanel;
    private final JScrollPane mWelcomeScreen;

    //
    private ComponentListener mComponentListener = new ComponentListener() {
        @Override
        public void componentResized(ComponentEvent e) {
            Preferences.setProperty("frame_height", Integer.toString(getHeight()));
            Preferences.setProperty("frame_width", Integer.toString(getWidth()));
            Preferences.save();
        }

        @Override
        public void componentMoved(ComponentEvent e) {
            Preferences.setProperty("frame_posx", Integer.toString(getX()));
            Preferences.setProperty("frame_posy", Integer.toString(getY()));
            Preferences.save();
        }

        @Override
        public void componentShown(ComponentEvent e) {
        }

        @Override
        public void componentHidden(ComponentEvent e) {
        }
    };

    // Private construction of an editor
    private EditorInstance() {
        Preferences.configure();

        // Load the preferences
        Preferences.load();
        getContentPane().setBackground(Color.WHITE);
        try {
            UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (final Exception e) {
            // If Nimbus is not available, you can set the GUI to another look and feel.
        }
        // SET FONTS
        setUIFonts();

        // SET BACKGROUNDS
        setUIBackgrounds();

        // Preferences.info();
        // Init the menu bar
        mMenuBar = new MenuBar(this);
        // Hide the menu bar 
        mMenuBar.setVisible(false);

        // Init the project editor list
        mProjectEditorList = new ProjectEditorList();
        mObservable.addObserver(mProjectEditorList);

        // Init welcome screen
        mWelcomePanel = new WelcomePanel(this);
        mWelcomeScreen = new JScrollPane(mWelcomePanel);
        mWelcomeScreen.setMaximumSize(java.awt.Toolkit.getDefaultToolkit().getScreenSize());
        mWelcomeScreen.setOpaque(false);
        mWelcomeScreen.getViewport().setOpaque(false);
        add(mWelcomeScreen);
        setIconImage(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png").getImage());

        // Init the windows closing support
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent event) {
                exit();
            }
        });

        // Init the editor application frame
        // TODO: static property fields
        Dimension editorSize = new Dimension(Integer.valueOf(Preferences.getProperty("frame_width")),
                Integer.valueOf(Preferences.getProperty("frame_height")));

        setPreferredSize(editorSize);
        setSize(editorSize);
        checkAndSetLocation();
        setTitle(Preferences.getProperty("frame_title"));
        setName(Preferences.getProperty("frame_name"));
        setJMenuBar(mMenuBar);
        // setContentPane(jsWelcome);
        // add(mProjectEditorList); // COMMENTED BY M.FALLAS
        pack();

        // handle resize and positioning
        this.addComponentListener(mComponentListener);

        // Register the editor as event listener
        mEventCaster.append(this);
    }

    private void setUIFonts() {
        String defaultFont = "Helvetica";    // DEFAULT FONT FOR ALL COMPONENTS

        UIManager.put("Button.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ToggleButton.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("RadioButton.font", new Font(defaultFont, Font.PLAIN, 14));

        UIManager.put("CheckBox.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ColorChooser.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ComboBox.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Label.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("List.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("MenuBar.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("MenuItem.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("RadioButtonMenuItem.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("CheckBoxMenuItem.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Menu.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("PopupMenu.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("OptionPane.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Panel.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ProgressBar.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ScrollPane.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Viewport.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TabbedPane.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Table.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TableHeader.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TextField.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("PasswordField.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TextArea.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TextPane.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("EditorPane.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("TitledBorder.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ToolBar.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("ToolTip.font", new Font(defaultFont, Font.PLAIN, 14));
        UIManager.put("Tree.font", new Font(defaultFont, Font.PLAIN, 10));
    }

    private void setUIBackgrounds() {
        UIManager.put("Frame.background", Color.WHITE);
        UIManager.put("Panel.background", Color.WHITE);
        UIManager.put("MenuBar.opaque", true);
        UIManager.put("MenuBar.background", Color.WHITE);
        UIManager.put("Menu.opaque", true);
        UIManager.put("Menu.background", Color.WHITE);
        UIManager.put("MenuItem.opaque", true);
        UIManager.put("MenuItem.background", Color.WHITE);
        UIManager.put("ToolBar.opaque", true);
        UIManager.put("ToolBar.background", Color.WHITE);
        UIManager.put("TabbedPane.background", Color.WHITE);
        UIManager.put("EditorPane.background", Color.WHITE);
        UIManager.put("ScrollPane.background", Color.WHITE);
        UIManager.put("Viewport.background", Color.WHITE);
    }

    public void update() {
        if (mProjectEditorList.getTabCount() > 0) {
            mObservable.update(mProjectEditorList.getSelectedEditorProject());
        }
    }

    /**
     * Shows or hides project editor list. Used by the welcome screen
     *
     * @param state
     */
    /*
     public void toggleProjectEditorList(boolean state) {
     if (state) {
     add(mProjectEditorList);
     remove(jsWelcome);
     } else {
     add(jsWelcome);
     remove(mProjectEditorList);
     jsWelcome.update(jsWelcome.getGraphics());
     }

     this.update(this.getGraphics());
     }
     */
    public void clearRecentProjects() {
        mWelcomePanel.updateWelcomePanel();
    }

    public synchronized static EditorInstance getInstance() {
        if (sInstance == null) {
            sInstance = new EditorInstance();
        }

        return sInstance;
    }

    @Override
    public void update(final EventObject event) {
        if (event instanceof AbortEvent) {
            ErrorDialog errorDialog = ErrorDialog.getInstance();

            errorDialog.addError((AbortEvent) event);
            errorDialog.setVisible(true);
        }
    }

    private void checkAndSetLocation() {
        Point finalPos = new Point(0, 0);
        Point editorPosition = new Point(Integer.valueOf(Preferences.getProperty("frame_posx")),
                Integer.valueOf(Preferences.getProperty("frame_posy")));
        Dimension editorSize = new Dimension(Integer.valueOf(Preferences.getProperty("frame_width")),
                Integer.valueOf(Preferences.getProperty("frame_height")));

        // check systems monitor setup
        Rectangle virtualBounds = new Rectangle();
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gs = ge.getScreenDevices();

        for (int j = 0; j < gs.length; j++) {
            GraphicsDevice gd = gs[j];
            GraphicsConfiguration[] gc = gd.getConfigurations();

            for (int i = 0; i < gc.length; i++) {

                // check position
                if (((editorPosition.x > gc[i].getBounds().x) && (gc[i].getBounds().width > editorSize.width))
                        && ((editorPosition.y > gc[i].getBounds().y)
                        && (gc[i].getBounds().height > editorSize.height))) {

                    // component can be place there
                    finalPos = new Point(editorPosition.x, editorPosition.y);

                    break;
                }
            }
        }

        setLocation(finalPos);
    }

    // Get the project editor list
    public final ProjectEditorList getProjectEditorList() {
        return mProjectEditorList;
    }

    // Get the current project editor
    public final ProjectEditor getSelectedProjectEditor() {
        return mProjectEditorList.getSelectedProjectEditor();
    }

    // Get the current editor project
    public final EditorProject getSelectedEditorProject() {
        return mProjectEditorList.getSelectedEditorProject();
    }

    // Create a new editor project
    public final void newProject() {
        // Create a new empty project
        final EditorProject project = new EditorProject();
        // Toggle the editor main screen
        if (mProjectEditorList.isEmpty()) {
            // Show the project editors
            setContentPane(mProjectEditorList);
            // Show the menu bar items
            mMenuBar.setVisible(true);
        }
        // Add the project to the editors
        mProjectEditorList.append(project);
    }

    // Open a new project with a file chooser
    public final boolean openProject() {
        // Create a new file chooser
        final JFileChooser chooser = new JFileChooser(System.getProperty("user.dir"));
        // Configure The File Chooser
        // TODO: Set the correct view and filter
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        // Show the file chooser in open mode 
        final int option = chooser.showOpenDialog(this);
        // Check the result of the file chooser
        if (option == JFileChooser.APPROVE_OPTION) {
            // Get the chooser's selected file 
            final File file = chooser.getSelectedFile();
            // And try to open the file then
            return openProject(file);
        } else {
            // Print an error message
            mLogger.warning("Warning: Canceled opening of a project file");
            // And return failure here
            return false;
        }
    }

    // Open an editor project from a file
    public final boolean openProject(final File file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot open editor project with bad file");
            // And return failure here
            return false;
        }

        // Check if the file exists
        if (!file.exists()) {
            // Print an error message
            mLogger.failure("Error: Cannot find editor project file '" + file + "'");
            // And return failure here
            return false;
        }

        // Create a new editor project 
        final EditorProject project = new EditorProject();
        // Try to load it from the file
        if (project.load(file)) {
            // Toggle the editor main screen
            if (mProjectEditorList.isEmpty()) {
                // Show the project editors
                setContentPane(mProjectEditorList);
                // Show the menu bar items
                mMenuBar.setVisible(true);
            }
            // Add the project to the editors
            mProjectEditorList.append(project);
            // Update the recent project list
            updateRecentProjects(
                    project.getProjectPath(),
                    project.getProjectName());
            // Print some info message
            mLogger.message("Opening project editor from file '" + file + "'");
            // Return true at success
            return true;
        } else {
            // Print an error message
            mLogger.failure("Error: Cannot load editor project from file '" + file + "'");
            // Return false at failure
            return false;
        }
    }

    // Close the current project editor
    public final void closeProject() {
        // Close the current project editor
        mProjectEditorList.closeProject();
        // Toggle the editor main screen
        if (mProjectEditorList.isEmpty()) {
            // Show the project editors
            setContentPane(mWelcomeScreen);
            // Hide the menu bar items
            mMenuBar.setVisible(false);
        }
    }

    public void saveAllProjects() {
        mProjectEditorList.saveAll();
        update();
    }

    public final boolean save() {
        if (!mProjectEditorList.save()) {
            return false;
        }

        // TODO: NO UPDATE IF NO SUCCES AT SAVING SCENES
        update();

        // update rectent project list
        updateRecentProjects(
                mProjectEditorList.getSelectedEditorProject().getProjectPath(),
                mProjectEditorList.getSelectedEditorProject().getProjectName());

        // TODO: Refresh the recent file menu
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    public void saveFileAs() {
        // Show A Project Creation Dialog
        final CreateProjectDialog dialog = new CreateProjectDialog();
        // Get Currently Selected Project
        final EditorProject project = mProjectEditorList.getSelectedEditorProject();
        // Set The New Project Directory

        // TODO: remove getter method
        project.setProjectFile(dialog.mProjectDir);
        //dialog.mConfigFile.getName(),
        //dialog.mConfigFile.getPath());
        mProjectEditorList.save();
        mProjectEditorList.setTitleAt(mProjectEditorList.getSelectedIndex(), dialog.mProjectName);
        mProjectEditorList.repaint();

        // update rectent project list
        updateRecentProjects(dialog.mProjectDir.getPath(), dialog.mProjectName);
    }

    public void exit() {

        // Remove all observers of the editor
        mObservable.deleteObservers();
        // Close the whole list of project editors
        mProjectEditorList.closeAll();
        System.exit(0);
    }

    public void updateRecentProjects(String projectDir, String projectName) {
        // Print some info message
        mLogger.message("Updating recent projects with '" + projectDir + "' '" + projectName + "'");

        ArrayList<String> recentProjectDirs = new ArrayList<String>();
        ArrayList<String> recentProjectNames = new ArrayList<String>();

        for (int i = 0; i <= Preferences.sMAX_RECENT_PROJECTS; i++) {
            String pDir = Preferences.getProperty("recentprojectdir" + i);

            if (pDir != null) {
                if (pDir.startsWith("res" + System.getProperty("file.separator") + "prj")) {
                    continue;
                }

                recentProjectDirs.add(Preferences.getProperty("recentprojectdir" + i));
            }

            String pName = Preferences.getProperty("recentprojectname" + i);

            if (pName != null) {
                recentProjectNames.add(Preferences.getProperty("recentprojectname" + i));
            }
        }

        if (recentProjectDirs.contains(projectDir)) {

            // case: project in recent list
            if (recentProjectNames.contains(projectName)) {

                // case: project is on list - has now to be at first pos
                int index = recentProjectDirs.indexOf(projectDir);

                if (index != 0) {
                    recentProjectDirs.add(0, projectDir);
                    recentProjectNames.add(0, projectName);
                    recentProjectNames.remove(index + 1);
                }
            } else {

                // dir is the same, but name changed
                int index = recentProjectDirs.indexOf(projectDir);

                Preferences.setProperty("recentprojectname" + index, projectName);
                Preferences.setProperty("recentprojectdate" + index, new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
                recentProjectNames.remove(index);
                recentProjectNames.add(index, projectName);
            }
        } else {

            // case: project not in recent list
            recentProjectDirs.add(0, projectDir);
            recentProjectNames.add(0, projectName);
        }

        // set properties
        String dir = null;
        String name = null;
        int maxCnt = (recentProjectDirs.size() <= Preferences.sMAX_RECENT_PROJECTS)
                ? recentProjectDirs.size()
                : Preferences.sMAX_RECENT_PROJECTS;

        for (int i = 0; i < maxCnt; i++) {
            dir = recentProjectDirs.get(i);
            name = recentProjectNames.get(i);

            if ((dir != null) && (name != null)) {
                Preferences.setProperty("recentprojectdir" + i, dir);
                Preferences.setProperty("recentprojectname" + i, name);
                Preferences.setProperty("recentprojectdate" + i, new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
            } else {
                break;
            }
        }

        // save properties
        Preferences.save();
        mWelcomePanel.createListOfRecentProj();
        mMenuBar.refreshRecentFileMenu();
    }

    /**
     *
     *
     *
     *
     *
     */
    public void showOptions() {
        OptionsDialog optionsDialog = OptionsDialog.getInstance();

        optionsDialog.setVisible(true);
    }

    public void showMonitor() {
        MonitorDialog monitorDialog = MonitorDialog.getInstance();

        monitorDialog.setVisible(true);
    }

    public void showHelp() {
        AboutDialog aboutDialog = AboutDialog.getInstance();

        aboutDialog.setVisible(true);
    }

    public void showAbout() {
        AboutDialog aboutDialog = AboutDialog.getInstance();

        aboutDialog.setVisible(true);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public void startSceneFlow() {

        // Get the sceneflow that has to be executed
        EditorProject project = mProjectEditorList.getSelectedEditorProject();

        // SceneFlow sceneFlow = mEditorList.getSelectedProject().getSceneFlow();
        // Register the sceneflow with the sceneplayer at the runtime
        // Get the sceneplayer that has to be used
        // ScenePlayer scenePlayer = mEditorList.getSelectedProject().loadPluginScenePlayer();
        // mRunTime.registerSceneFlow(sceneFlow, scenePlayer);
        // mLogger.message("Registering Sceneflow " + project.getSceneFlow());
        mRunTime.launch(project);

        // Start the interpreter for that sceneflow
        // mLogger.message("Starting Sceneflow " + project.getSceneFlow());
        mRunTime.start(project.getSceneFlow());

        // Disable the project list
        mProjectEditorList.setEnabled(false);

        //
        // mEditorList.getSelectedProjectEditor().getSceneEditor().setEnabled(false);
        // Update the menu bar
//      mMenuBar.setRunMenuEnabled(false);
//      mMenuBar.setStopMenuEnabled(true);
//      mMenuBar.setPauseMenuEnabled(true);
//      mMenuBar.setMonitorMenuEnabled(true);
    }

    // Start the sceneflow of the current project
    public final void stopSceneFlow() {
        // Get the current project
        final EditorProject project = mProjectEditorList.getSelectedEditorProject();

        // Stop the interpreter for that sceneflow
        // mRunTime.stopSceneFlow(sceneFlow);
        mRunTime.abort(project.getSceneFlow());

        // mLogger.message("Stopping Sceneflow " + project.getSceneFlow());
        //
        mRunTime.unload(project);

        // mLogger.message("Unregistering Sceneflow " + project.getSceneFlow());
        // Enable the project list
        mProjectEditorList.setEnabled(true);

        //
        // mEditorList.getSelectedProjectEditor().getSceneEditor().setEnabled(true);
        //
//      mMenuBar.setRunMenuEnabled(true);
//      mMenuBar.setStopMenuEnabled(false);
//      mMenuBar.setPauseMenuEnabled(false);
//      mMenuBar.setMonitorMenuEnabled(false);
        update();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public void pauseSceneFlow() {
        SceneFlow sceneFlow = mProjectEditorList.getSelectedEditorProject().getSceneFlow();

        if (mRunTime.isPaused(sceneFlow)) {
            mRunTime.proceed(sceneFlow);
        } else {
            mRunTime.pause(sceneFlow);
        }

        if (mRunTime.isPaused(sceneFlow)) {

            // mMenuBar.setRunMenuEnabled(false);
            // mMenuBar.setStopMenuEnabled(false);
            // mMenuBar.setMonitorMenuEnabled(false);
            // mMenuBar.setPauseMenuText("Proceed");
        } else {

            // mMenuBar.setStopMenuEnabled(true);
            // mMenuBar.setMonitorMenuEnabled(true);
            // mMenuBar.setPauseMenuText("Pause");
        }
    }
}
