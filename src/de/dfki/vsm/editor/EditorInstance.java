package de.dfki.vsm.editor;

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.ProjectEditor;
import de.dfki.vsm.editor.dialog.AboutDialog;
import de.dfki.vsm.editor.dialog.ErrorDialog;
import de.dfki.vsm.editor.dialog.MonitorDialog;
import de.dfki.vsm.editor.dialog.OptionsDialog;
import de.dfki.vsm.editor.dialog.QuitDialog;
import de.dfki.vsm.editor.event.SceneStoppedEvent;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.runtime.events.AbortionEvent;
import de.dfki.vsm.util.evt.EventDispatcher;
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
import javax.swing.JTabbedPane;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * @author Gregor Mehlmann
 */
public final class EditorInstance extends JFrame implements EventListener, ChangeListener {

    // The singelton editor instance
    public static EditorInstance sInstance = null;
    // The singelton runtime instance 
    private final RunTimeInstance mRunTime = RunTimeInstance.getInstance();
    // The singelton logger instance   
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton event multicaster
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();
    // The editor's GUI components
    private final EditorMenuBar mEditorMenuBar;
    private final JTabbedPane mProjectEditors;
    private final JScrollPane mWelcomeScreen;
    private final EditorStarter mWelcomePanel;

    // Get the singelton editor instance
    public synchronized static EditorInstance getInstance() {
        if (sInstance == null) {
            sInstance = new EditorInstance();
        }

        return sInstance;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    WorkSpacePanel.ClipBoard previousCB = null;

    @Override
    public void stateChanged(ChangeEvent e) {
        if (getSelectedProjectEditor().getEditorProject() != null) {
            getSelectedProjectEditor().refresh();
            //mObservable.update(getSelectedProjectEditor().getEditorProject());
        }

        // copy and paste of nodes between the different projects
        ProjectEditor projectEditor = ((ProjectEditor) mProjectEditors.getSelectedComponent());

        if (projectEditor != null) {
            if (previousCB != null) {
                WorkSpacePanel.ClipBoard currentCB = projectEditor.getSceneFlowEditor().getWorkSpace().getClipBoard();

                currentCB.clear();

                for (Node node : previousCB) {
                    currentCB.add(node);
                }
            }

            previousCB = projectEditor.getSceneFlowEditor().getWorkSpace().getClipBoard();
        }
    }

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
        mEditorMenuBar = new EditorMenuBar(this);
        // Hide the menu bar 
        mEditorMenuBar.setVisible(false);

        // Init the project editor list
        mProjectEditors = new JTabbedPane(
                JTabbedPane.TOP, JTabbedPane.WRAP_TAB_LAYOUT);
        //mObservable.addObserver(mProjectEditors);

        // Init welcome screen
        mWelcomePanel = new EditorStarter(this);
        mWelcomeScreen = new JScrollPane(mWelcomePanel);
        mWelcomeScreen.setMaximumSize(java.awt.Toolkit.getDefaultToolkit().getScreenSize());
        mWelcomeScreen.setOpaque(false);
        mWelcomeScreen.getViewport().setOpaque(false);
        add(mWelcomeScreen);
        setIconImage(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png").getImage());
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        // Init the windows closing support
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent event) {
                // Close all project editors
                closeAll();
                // And finally exit the system
                //System.exit(0);
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
        setJMenuBar(mEditorMenuBar);
        // setContentPane(jsWelcome);
        // add(mProjectEditorList); // COMMENTED BY M.FALLAS
        pack();

        // handle resize and positioning
        this.addComponentListener(mComponentListener);

        // Register the editor as event listener
        mEventCaster.register(this);
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

    public void clearRecentProjects() {
        mWelcomePanel.updateWelcomePanel();
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

    ////////////////////////////////////////////////////////////////////////////
    // Get the current project editor
    public final ProjectEditor getSelectedProjectEditor() {
        return (ProjectEditor) mProjectEditors.getSelectedComponent();
    }

    ////////////////////////////////////////////////////////////////////////////
    // Get the current project editor
    public final JTabbedPane getProjectEditors() {
        return mProjectEditors;
    }

    public final boolean newProject() {
        // Create a new project editor
        final ProjectEditor editor = new ProjectEditor();
        // Add the new project editor 
        mProjectEditors.addTab("", editor);
        mProjectEditors.setSelectedComponent(editor);
        // Show the editor projects now
        if (mProjectEditors.getTabCount() == 1) {
            // Show the project editors
            setContentPane(mProjectEditors);
            // Show the menu bar items
            mEditorMenuBar.setVisible(true);
        }
        // Refresh the appearance
        refresh();
        // Return true at success
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    // Open a new project editor with chooser
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

    // Open an project editor from a file
    public final boolean openProject(final File file) {
        // Check if the file is null
        if (file == null) {
            // Print an error message
            mLogger.failure("Error: Cannot open editor project from a bad file");
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
        if (project.parse(file)) {
            // Toggle the editor main screen
            if (mProjectEditors.getTabCount() == 0) {
                // Show the project editors
                setContentPane(mProjectEditors);
                // Show the menu bar items
                mEditorMenuBar.setVisible(true);
            }
            // Create a new project editor from project
            final ProjectEditor projectEditor = new ProjectEditor(project);
            // Add the project editor to list of project 
            // editors and select it in the tabbed pane
            mProjectEditors.addTab(project.getProjectName(), projectEditor);
            mProjectEditors.setSelectedComponent(projectEditor);
            // Update the recent project list
            updateRecentProjects(project);
            // Print some info message
            mLogger.message("Opening project editor from file '" + file + "'");
            // Refresh the appearance
            refresh();
            // Return true at success
            return true;
        } else {
            // Print an error message
            mLogger.failure("Error: Cannot load editor project from file '" + file + "'");
            // Return false at failure
            return false;
        }

    }

    // Save the selected project editor
    public final boolean save() {
        return save(getSelectedProjectEditor());
    }

    // Save the specific project editor 
    public final boolean save(final ProjectEditor editor) {
        // Check if the editor is valid
        if (editor != null) {
            // Get the selected editor project
            final EditorProject project = editor.getEditorProject();
            // Check if the project is valid
            if (project != null) {
                // Check if the project is pending
                if (!project.isPending()) {
                    // Try to write the editor project
                    if (project.write()) {
                        // Refresh the title of the project tab
                        final int index = mProjectEditors.getSelectedIndex();
                        final String title = mProjectEditors.getTitleAt(index);
                        mProjectEditors.setTitleAt(index, title.replace("*", ""));
                        // Update rectent project list
                        updateRecentProjects(project);
                        // Refresh the appearance
                        refresh();
                        // Return true at success
                        return true;
                    } else {
                        // Print an error message
                        mLogger.failure("Error: Cannot write the editor project '" + project + "'");
                        // And return failure here
                        return false;
                    }
                } else {
                    // Choose a new file to save to
                    return saveAs(editor);
                }
            } else {
                // Print an error message
                mLogger.failure("Error: Cannot save a bad editor project");
                // And return failure here
                return false;
            }
        } else {
            // Print an error message
            mLogger.failure("Error: Cannot save a bad project editor");
            // And return failure here
            return false;
        }
    }

    // Save the selected project editor 
    public final boolean saveAs() {
        return saveAs(getSelectedProjectEditor());
    }

    // Save the specific project editor 
    public final boolean saveAs(final ProjectEditor editor) {
        // Check if the editor is valid
        if (editor != null) {
            // Get currently selected project
            final EditorProject project = editor.getEditorProject();
            // Check if the project is valid
            if (project != null) {
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
                    // Try to write the editor project
                    if (project.write(file)) {
                        // Refresh the title of the project tab
                        final int index = mProjectEditors.getSelectedIndex();
                        mProjectEditors.setTitleAt(index, project.getProjectName());
                        // Update rectent project list
                        updateRecentProjects(project);
                        // Refresh the appearance
                        refresh();
                        // Return true at success
                        return true;
                    } else {
                        // Print an error message
                        mLogger.failure("Error: Cannot write the editor project '" + project + "'");
                        // And return failure here
                        return false;
                    }
                } else {
                    // Print an error message
                    mLogger.warning("Warning: Canceled saving of a project file");
                    // And return failure here
                    return false;
                }
            } else {
                // Print an error message
                mLogger.failure("Error: Cannot save a bad editor project");
                // And return failure here
                return false;
            }
        } else {
            // Print an error message
            mLogger.failure("Error: Cannot save a bad project editor");
            // And return failure here
            return false;
        }
    }

    // Close the selected project editor
    public final void close(int DialogType) {
        // Close the current project editor
        close(getSelectedProjectEditor(), DialogType);
    }

    // Close a specific project editor
    private int close(final ProjectEditor editor, int DialogType) {
        
        // Check if the project has changed
        if (editor.getEditorProject().hasChanged()) {

            QuitDialog qDiag = new QuitDialog(DialogType);
            int exitMessage = qDiag.getExitMessage();
            System.out.println(exitMessage);
            if (exitMessage == QuitDialog.CANCEL_CLOSING)
            {
                return exitMessage;
            }
            if (exitMessage == QuitDialog.SAVE_AND_EXIT)
            {
                save(editor);
            }
            // Close the project editor itself
            editor.close();
            // Remove the component 
            mProjectEditors.remove(editor);
            // Toggle the editor main screen
            if (mProjectEditors.getTabCount() == 0) {
                // Show the project editors
                setContentPane(mWelcomeScreen);
                // Hide the menu bar items
                mEditorMenuBar.setVisible(false);
            }
            // Refresh the appearance
            refresh();
            return exitMessage;

        }
        return QuitDialog.SAVE_AND_EXIT;
        
    }

    // Save all project editors
    public final void saveAll() {
        for (int i = 0; i < mProjectEditors.getTabCount(); i++) {
            save(((ProjectEditor) mProjectEditors.getComponentAt(i)));
        }
    }

    // Close all project editors
    public final void closeAll() {
        for (int i = 0; i < mProjectEditors.getTabCount(); i++) {
            int result = close((ProjectEditor) mProjectEditors.getComponentAt(i), QuitDialog.EXIT_DIALOG);
            if (result == QuitDialog.CANCEL_CLOSING)
            {
                return;
            }
        }
        System.exit(0);
    }

    /*

     private final void refreshRecentProjects(final EditorProject project) throws ParseException {
     final String projectPath = project.getProjectPath();
     final String projectName = project.getProjectName();
     // Create the list of recent projects
     final ArrayList<TPLTriple<String, String, Date>> projects = new ArrayList<>();
     // Get all remembered recent projects
     for (int i = 0; i <= Preferences.sMAX_RECENT_PROJECTS; i++) {
     final String path = Preferences.getProperty("recentproject." + i + ".path");
     final String name = Preferences.getProperty("recentproject." + i + ".name");
     final Date date = new SimpleDateFormat("dd.MM.yyyy").parse(
     Preferences.getProperty("recentproject." + i + ".date"));
     // Create the current recent project
     TPLTriple<String, String, Date> recent = new TPLTriple(name, path, date);
     //
     projects.add(recent);
     }
     // Interate over the recent projects
     for (TPLTriple<String, String, Date> recent : projects) {
     //if () {

     //}
     }

     }*/
    // Update list of recent projects
    public void updateRecentProjects(final EditorProject project) {
        final String projectPath = project.getProjectPath();
        final String projectName = project.getProjectName();
        // Print some info message
        mLogger.message("Updating recent projects with '" + projectPath + "' '" + projectName + "'");
        //
        ArrayList<String> paths = new ArrayList<>();
        ArrayList<String> names = new ArrayList<>();
        // Load the list of recent projects
        for (int i = 0; i <= Preferences.sMAX_RECENT_PROJECTS; i++) {
            final String path = Preferences.getProperty("recentproject." + i + ".path");

            if (path != null) {
                // TODO: hardcoding this is bad style
                if (path.startsWith("res" + System.getProperty("file.separator") + "prj")) {
                    continue;
                }

                paths.add(Preferences.getProperty("recentproject." + i + ".path"));
            }

            String pName = Preferences.getProperty("recentproject." + i + ".name");

            if (pName != null) {
                names.add(Preferences.getProperty("recentproject." + i + ".name"));
            }
        }

        if (paths.contains(projectPath)) {

            // case: project in recent list
            if (names.contains(projectName)) {

                // case: project is on list - has now to be at first pos
                int index = paths.indexOf(projectPath);

                if (index != 0) {
                    paths.add(0, projectPath);
                    names.add(0, projectName);
                    names.remove(index + 1);
                }
            } else {

                // dir is the same, but name changed
                int index = paths.indexOf(projectPath);

                Preferences.setProperty("recentproject." + index + ".name", projectName);
                Preferences.setProperty("recentproject." + index + ".date", new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
                names.remove(index);
                names.add(index, projectName);
            }
        } else {

            // case: project not in recent list
            paths.add(0, projectPath);
            names.add(0, projectName);
        }

        // set properties
        String dir = null;
        String name = null;
        int maxCnt = (paths.size() <= Preferences.sMAX_RECENT_PROJECTS)
                ? paths.size()
                : Preferences.sMAX_RECENT_PROJECTS;

        for (int i = 0; i < maxCnt; i++) {
            dir = paths.get(i);
            name = names.get(i);

            if ((dir != null) && (name != null)) {
                Preferences.setProperty("recentproject." + i + ".path", dir);
                Preferences.setProperty("recentproject." + i + ".name", name);
                Preferences.setProperty("recentproject." + i + ".date", new SimpleDateFormat("dd.MM.yyyy").format(new Date()));
            } else {
                break;
            }
        }

        // save properties
        Preferences.save();
        mWelcomePanel.createListOfRecentProj();
        mEditorMenuBar.refreshRecentFileMenu();
    }

    // Show the options dialog 
    public final void showOptions() {
        final OptionsDialog optionsDialog = OptionsDialog.getInstance();

        optionsDialog.setVisible(true);
    }

    // Show the monitor dialog
    public final void showMonitor() {
        final MonitorDialog monitorDialog = MonitorDialog.getInstance();

        monitorDialog.setVisible(true);
    }

    // Show the help dialog
    public final void showHelp() {
        final AboutDialog aboutDialog = AboutDialog.getInstance();

        aboutDialog.setVisible(true);
    }

    // Show the about dialog
    public final void showAbout() {
        final AboutDialog aboutDialog = AboutDialog.getInstance();

        aboutDialog.setVisible(true);
    }

    // Play the execution of the current project
    public final void play() {
        // Get the project that has to be executed
        final ProjectEditor editor = getSelectedProjectEditor();
        final EditorProject project = editor.getEditorProject();
        // Launch the current project in the runtime
        play(project);
    }

    // Play the execution of the current project
    public final boolean play(final EditorProject project) {
        // Check State Of Execution
        if (mRunTime.isRunning(project)) {
            if (mRunTime.isPaused(project)) {
                if (mRunTime.proceed(project)) {
                    // Print some information
                    mLogger.message("Proceeding project '" + project + "'");
                    // Refresh the appearance
                    refresh();
                    // Return true at success
                    return true;
                } else {
                    // Print an error message
                    mLogger.failure("Error: Cannot proceed project '" + project + "'");
                    // Return false at failure
                    return false;
                }
            } else {
                if (mRunTime.pause(project)) {
                    // Print some information
                    mLogger.message("Pausing project '" + project + "'");
                    // Refresh the appearance
                    refresh();
                    // Return true at success
                    return true;
                } else {
                    // Print an error message
                    mLogger.failure("Error: Cannot pause project '" + project + "'");
                    // Return false at failure
                    return false;
                }
            }
        } else {
            // Launch the current project in the runtime
            if (mRunTime.launch(project)) {
                // Print some information
                mLogger.message("Launching project '" + project + "'");
                // Start the interpreter for that project
                if (mRunTime.start(project)) {
                    // Print some information
                    mLogger.message("Starting project '" + project + "'");
                    // Refresh the appearance
                    refresh();
                    // Return true at success
                    return true;
                } else {
                    // Print an error message
                    mLogger.failure("Error: Cannot start project '" + project + "'");
                    // Return false at failure
                    return false;
                }
            } else {
                // Print an error message
                mLogger.failure("Error: Cannot launch project '" + project + "'");
                // Return false at failure
                return false;
            }
        }
    }

    // Stop the execution of a specific project
    public final boolean stop(final EditorProject project) {
        // Abort the interpreter for that project
        if (mRunTime.abort(project)) {
            // Print some information
            mLogger.message("Aborting project '" + project + "'");
            // Unload the current project in the runtime
            if (mRunTime.unload(project)) {
                // Print some information
                mLogger.message("Unloading project '" + project + "'");
                // Launch event to unselect nodes and edges
                mEventCaster.convey(new SceneStoppedEvent(this));
                // Refresh the appearance
                refresh();
                // Return true at success
                return true;
            } else {
                // Print an error message
                mLogger.failure("Error: Cannot unload project '" + project + "'");
                // Return false at failure
                return false;
            }
        } else {
            // Print an error message
            mLogger.failure("Error: Cannot abort project '" + project + "'");
            // Return false at failure
            return false;
        }
    }

    // Update whenever an event has happened
    @Override
    public final void update(final EventObject event) {
        if (event instanceof AbortionEvent) {
            // Get the error dialog
            final ErrorDialog errorDialog = ErrorDialog.getInstance();
            // Appen the error message
            errorDialog.addError((AbortionEvent) event);
            // Show the error dialog
            errorDialog.setVisible(true);
        }
    }

    // Refresh this editor component
    public final void refresh() {
        // Print some information
        mLogger.message("Refreshing '" + this + "'");
        // Get the selected project editor
        final ProjectEditor editor = getSelectedProjectEditor();
        // Refresh the selected project editor
        if (editor != null) {
            editor.refresh();
        }
        // Refresh the editor's menu bar
        mEditorMenuBar.refresh();
        // Refresh editor welcome panel
        mWelcomePanel.refresh();
    }
}
