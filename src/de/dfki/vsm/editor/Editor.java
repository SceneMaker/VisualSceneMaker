package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.AboutDialog;
import de.dfki.vsm.editor.dialog.CreateProjectDialog;
import de.dfki.vsm.editor.dialog.ErrorDialog;
import de.dfki.vsm.editor.dialog.MonitorDialog;
import de.dfki.vsm.editor.dialog.OptionsDialog;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.runtime.RunTime;
import de.dfki.vsm.runtime.event.AbortEvent;
import de.dfki.vsm.util.cpy.CopyTool;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class Editor extends JFrame implements EventListener {

    /**
     * The runtime interface to the interpreter which is needed to register and
     * unregister SceneMaker projects and to start or stop the execution of a
     * SceneFlow.
     */
    private final RunTime mRunTime = RunTime.getInstance();
    /**
     * The SceneMaker logger which is used to log information or warnings to the
     * console or to a log file.
     */
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    /**
     * The SceneMaker event multicaster which is used to broadcast events
     * between the SceneMaker components. The Editor itself implements the
     * interface EventListener.
     */
    /**
     * The editor's observable component which is used to propagate events to
     * the editor's observers.
     */
    private final Observable mObservable = new Observable();
    /**
     * The singelton editor instance
     */
    private static Editor sInstance = null;
    /**
     * The editor's GUI components
     */
    private final MenuBar mMenuBar;
    private final ProjectEditorList mProjectEditorList;
    private ComponentListener mComponentListener = new ComponentListener() {
        public void componentResized(ComponentEvent e) {
            Preferences.setProperty("frame_height", Integer.toString(getHeight()));
            Preferences.setProperty("frame_width", Integer.toString(getWidth()));
            Preferences.save();
        }

        public void componentMoved(ComponentEvent e) {
            Preferences.setProperty("frame_posx", Integer.toString(getX()));
            Preferences.setProperty("frame_posy", Integer.toString(getY()));
            Preferences.save();
        }

        public void componentShown(ComponentEvent e) {
        }

        public void componentHidden(ComponentEvent e) {
        }
    };

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private class Observable extends java.util.Observable {

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }

    public void update() {
        if (mProjectEditorList.getTabCount() > 0) {
            mObservable.update(mProjectEditorList.getSelectedProject());
        }
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    private Editor() {
        Preferences.configure();
        // Load the preferences
        Preferences.load();
        //Preferences.info();
        // Init the menu bar
        mMenuBar = new MenuBar(this);
        mMenuBar.setFileSaveMenuEnabled(false);
        mMenuBar.setCloseMenuEnabled(false);
        // Init the project editor list
        mProjectEditorList = new ProjectEditorList();
        mObservable.addObserver(mProjectEditorList);
        // Init the windows closing support
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent event) {
                exit();
            }
        });
        // Init the editor application frame
        // TODO: static property fields
        Dimension editorSize = new Dimension(
                Integer.valueOf(Preferences.getProperty("frame_width")),
                Integer.valueOf(Preferences.getProperty("frame_height")));
        setPreferredSize(editorSize);
        setSize(editorSize);
        checkAndSetLocation();
        setTitle(Preferences.getProperty("frame_title"));
        setName(Preferences.getProperty("frame_name"));
        setJMenuBar(mMenuBar);
        add(mProjectEditorList);
        pack();
        // handle resize and positioning
        this.addComponentListener(mComponentListener);
        // Register the editor as event listener
        mEventMulticaster.append(this);
    }

    public synchronized static Editor getInstance() {
        if (sInstance == null) {
            sInstance = new Editor();
        }
        return sInstance;
    }
    /**
     * *************************************************************************
     *
     *
     *************************************************************************
     */
    private final EventCaster mEventMulticaster
            = EventCaster.getInstance();

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
        Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

        Point editorPosition = new Point(
                Integer.valueOf(Preferences.getProperty("frame_posx")),
                Integer.valueOf(Preferences.getProperty("frame_posy")));

        Dimension editorSize = new Dimension(
                Integer.valueOf(Preferences.getProperty("frame_width")),
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
                        && ((editorPosition.y > gc[i].getBounds().y) && (gc[i].getBounds().height > editorSize.height))) {
                    // component can be place there
                    finalPos = new Point(editorPosition.x, editorPosition.y);
                    break;
                }
            }
        }
        setLocation(finalPos);
    }

    /**
     * *************************************************************************
     *
     *
     *************************************************************************
     */
    public ProjectEditor getSelectedProjectEditor() {
        return mProjectEditorList.getSelectedProjectEditor();
    }

    public ProjectEditorList getProjectEditorList() {
        return mProjectEditorList;
    }

    /**
     * *************************************************************************
     *
     *
     *************************************************************************
     */
    public void newProject() {
        CreateProjectDialog createProjectDialog = new CreateProjectDialog();
        if (createProjectDialog != null) {
            ProjectData project = new ProjectData(new File(createProjectDialog.mConfigFile.getPath()));
            project.setPending(true);
            addProject(project);
            // update rectent project list
            updateRecentProjects(createProjectDialog.mProjectDir.getPath(), createProjectDialog.mProjectName);
        }
    }

    public void openSceneflow() {
        // TODO
    }

    public void openProject() {
        
        final JFileChooser fc = new JFileChooser(System.getProperty("user.dir"));
    
       
        fc.setFileView(new OpenProjectView());
        
        fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        fc.setFileFilter(new FileFilter() {
            public boolean accept(File f) {
                if (f.isDirectory()) {               
                    
                    File configFile = new File(f.getPath() + System.getProperty("file.separator") + "config.xml");                   
                    if (configFile.exists()) {                        
                        return true;
                    }

                    File[] listOfFiles = f.listFiles();
                    for (File listOfFile : listOfFiles) {
                        if (listOfFile.isDirectory()) {
                            return true;
                        }
                    }
                    
                }
                return false;
            }

            public String getDescription() {
                return "SceneMaker Project File Filter";
            }
        });

        if (fc.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {

            if (new File(fc.getSelectedFile() + System.getProperty("file.separator"), "config.xml").exists()) {
                File configFile = new File(fc.getSelectedFile() + System.getProperty("file.separator") + "config.xml");
                ProjectData project = new ProjectData(configFile);
                addProject(project);
                // update rectent project list
                updateRecentProjects(project.getProjectDirPath(), project.getProjectName());
            } else {
                JOptionPane.showMessageDialog(this,
                        "Project Not Found.",
                        "Visual Scene Maker.",
                        JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    /**
     * Open project by project directory.
     *
     * @param file the project directory
     */
    public void openProject(File file) {
        File configFile = new File(file + System.getProperty("file.separator") + "config.xml");
        ProjectData project = new ProjectData(configFile);
        openProject(project);
    }

    /**
     * Open project by the project object.
     *
     * @param project the project object
     */
    public void openProject(ProjectData project) {
        addProject(project);
        // update recent project list
        updateRecentProjects(project.getProjectDirPath(), project.getProjectName());
    }

    public void updateHashValue() {
        mProjectEditorList.getSelectedProject().setSceneInitialHash(mProjectEditorList.getSelectedProject().getHashCode());
    }

    public void closeCurrentProject() {
        removeProject();
    }

    public void saveAllProjects() {
        mProjectEditorList.saveAll();
        update();
    }

    public ProjectData saveCurrentProject() {
        mProjectEditorList.saveCurrent();
        // TODO: NO UPDATE IF NO SUCCES AT SAVING SCENES
        update();
        // update rectent project list
        updateRecentProjects(
                mProjectEditorList.getSelectedProject().getProjectDirPath(),
                mProjectEditorList.getSelectedProject().getProjectName());
        // TODO: no return - what is pending -let project editor do this!
        return mProjectEditorList.getSelectedProject();
    }

    public void saveFileAs() {
        CreateProjectDialog createProjectDialog = new CreateProjectDialog();
        
        if (createProjectDialog != null) {
             
            ProjectData currentProject = mProjectEditorList.getSelectedProject();          
            currentProject.updateFileNames(createProjectDialog.mConfigFile.getName(), createProjectDialog.mConfigFile.getPath());
            mProjectEditorList.saveCurrent();             
            mProjectEditorList.setTitleAt(mProjectEditorList.getSelectedIndex(), createProjectDialog.mProjectName);
            mProjectEditorList.repaint();

             // update rectent project list
            updateRecentProjects(createProjectDialog.mProjectDir.getPath(), createProjectDialog.mProjectName);
        }
    }

    public void exit() {
        // Close the whole list of project editors
        mProjectEditorList.closeAll();

        System.exit(0);
    }

    private void addProject(ProjectData project) {
        if (mProjectEditorList.getTabCount() == 0) {
            mMenuBar.setFileSaveMenuEnabled(true);
            mMenuBar.setCloseMenuEnabled(true);
            //mMenuBar.setRunMenuEnabled(true);
            //mMenuBar.setMonitorMenuEnabled(false);

        }
        mProjectEditorList.add(project);
        project.setSceneInitialHash(project.getHashCode());
    }

    private void removeProject() {
        // Close the currently selected project editor
        mProjectEditorList.closeCurrent();
        // Update the recent file list
        //updateRecentFiles(sceneFlowFile);
        // Cleaning of used objects and threads
        if (mProjectEditorList.getTabCount() == 0) {
            mMenuBar.setFileSaveMenuEnabled(false);
            mMenuBar.setCloseMenuEnabled(false);
            //mMenuBar.setRunMenuEnabled(false);
            //mMenuBar.setStopMenuEnabled(false);
            //mMenuBar.setPauseMenuEnabled(false);
            //mMenuBar.setMonitorMenuEnabled(false);
        }
    }

    public void updateRecentProjects(String projectDir, String projectName) {
        ArrayList<String> recentProjectDirs = new ArrayList<String>();
        ArrayList<String> recentProjectNames = new ArrayList<String>();
        for (int i = 0; i <= Preferences.sMAX_RECENT_PROJECTS; i++) {
            String pDir = Preferences.getProperty("recentprojectdir" + i);
            if (pDir != null) {
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
        int maxCnt = (recentProjectDirs.size() <= Preferences.sMAX_RECENT_PROJECTS) ? recentProjectDirs.size() : Preferences.sMAX_RECENT_PROJECTS;
        for (int i = 0; i < maxCnt; i++) {
            dir = recentProjectDirs.get(i);
            name = recentProjectNames.get(i);
            if (dir != null && name != null) {
                Preferences.setProperty("recentprojectdir" + i, dir);
                Preferences.setProperty("recentprojectname" + i, name);
            } else {
                break;
            }
        }
        // save properties
        Preferences.save();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
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

    /**
     * *************************************************************************
     *
     *
     *************************************************************************
     */
    public void startSceneFlow() {
        // Get the sceneflow that has to be executed
        ProjectData project = mProjectEditorList.getSelectedProject();
        //SceneFlow sceneFlow = mEditorList.getSelectedProject().getSceneFlow();
        // Register the sceneflow with the sceneplayer at the runtime
        // Get the sceneplayer that has to be used
        //ScenePlayer scenePlayer = mEditorList.getSelectedProject().loadPluginScenePlayer();

        //mRunTime.registerSceneFlow(sceneFlow, scenePlayer);
        System.err.println("Registering Sceneflow " + project.getSceneFlow());
        mRunTime.register(project);
        // Start the interpreter for that sceneflow
        System.err.println("Starting sceneflow " + project.getSceneFlow());
        mRunTime.startSceneFlow(project.getSceneFlow());
        // Disable the project list
        mProjectEditorList.setEnabled(false);
        //
        //mEditorList.getSelectedProjectEditor().getSceneEditor().setEnabled(false);
        // Update the menu bar
//        mMenuBar.setRunMenuEnabled(false);
//        mMenuBar.setStopMenuEnabled(true);
//        mMenuBar.setPauseMenuEnabled(true);
//        mMenuBar.setMonitorMenuEnabled(true);
    }

    public void stopSceneFlow() {
        // Get the current project
        ProjectData project = mProjectEditorList.getSelectedProject();
        // Stop the interpreter for that sceneflow
        //mRunTime.stopSceneFlow(sceneFlow);
        mRunTime.stopSceneFlow(project.getSceneFlow());
        //
        mRunTime.unregister(project);
        // Enable the project list
        mProjectEditorList.setEnabled(true);
        //
        //mEditorList.getSelectedProjectEditor().getSceneEditor().setEnabled(true);
        //
//        mMenuBar.setRunMenuEnabled(true);
//        mMenuBar.setStopMenuEnabled(false);
//        mMenuBar.setPauseMenuEnabled(false);
//        mMenuBar.setMonitorMenuEnabled(false);

        update();

    }

    public void pauseSceneFlow() {
        SceneFlow sceneFlow = mProjectEditorList.getSelectedProject().getSceneFlow();

        if (mRunTime.isSceneFlowPaused(sceneFlow)) {
            mRunTime.proceedSceneFlow(sceneFlow);
        } else {
            mRunTime.pauseSceneFlow(sceneFlow);
        }

        if (mRunTime.isSceneFlowPaused(sceneFlow)) {
            //mMenuBar.setRunMenuEnabled(false);
            //mMenuBar.setStopMenuEnabled(false);
            //mMenuBar.setMonitorMenuEnabled(false);
            //mMenuBar.setPauseMenuText("Proceed");
        } else {
            //mMenuBar.setStopMenuEnabled(true);
            //mMenuBar.setMonitorMenuEnabled(true);
            //mMenuBar.setPauseMenuText("Pause");
        }

    }
}
