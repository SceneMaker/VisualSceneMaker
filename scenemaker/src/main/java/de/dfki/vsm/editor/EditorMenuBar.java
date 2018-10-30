package de.dfki.vsm.editor;

import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.dialog.NewProjectDialog;
import de.dfki.vsm.editor.dialog.QuitDialog;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;

/**
 * @author Gregor Mehlmann
 */
public final class EditorMenuBar extends JMenuBar {

    // The singelton logger instance   
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    // The singelton  editor instance
    private final EditorInstance mEditorInstance;

    // File menu
    private JMenu mFileMenu;
    private JMenuItem mCreateFileMenuItem;
    private JMenuItem mOpenFileMenuItem;
    private JMenuItem mOpenRecentFileMenu;
    private JMenuItem mClearRecentFileMenuItem;
    private JMenuItem mCloseFileMenuItem;
    private JMenuItem mSaveFileMenuItem;
    private JMenuItem mSaveAsMenuItem;
    private JMenuItem mSaveAllMenuItem;
    private JMenuItem mExitEditorMenuItem;
    // Edit menu
    private JMenu mEditMenu;
    private JMenuItem mCutMenuItem;
    private JMenuItem mCopyMenuItem;
    private JMenuItem mPasteMenuItem;
    private JMenuItem mDeleteMenuItem;
    private JMenuItem mStraightenMenuItem;
    private JMenuItem mNormalizeMenuItem;
    private JMenuItem mOptionsMenuItem;
    // Help menu
    private JMenu mHelpMenu;
    private JMenuItem mQuestionMenuItem;
    private JMenuItem mInfoMenuItem;

    // Construct the editor's menu bar
    public EditorMenuBar(final EditorInstance editor) {
        // Initialize the parent editor
        mEditorInstance = editor;
        // Initialize the GUI components
        initComponents();
    }

    // Refresh the visual appearance
    public final void refresh() {
        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
        // TODO: Refresh the appearance, i.e. the recent file menu
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Graphics2D g2d = (Graphics2D) g;

        g2d.setColor(Color.WHITE);
        g2d.fillRect(0, 0, getWidth() - 1, getHeight() - 1);
    }

    public void setCloseMenuEnabled(boolean flag) {
        mCloseFileMenuItem.setEnabled(flag);
    }

    public void setSaveMenusEnabled(boolean flag) {
        mSaveFileMenuItem.setEnabled(flag);
        mSaveAsMenuItem.setEnabled(flag);
        mSaveAllMenuItem.setEnabled(flag);
    }

    private void initComponents() {
        initFileMenu();
        initEditMenu();
        initHelpMenu();
    }

    public void refreshRecentFileMenu() {
        mOpenRecentFileMenu.removeAll();

        boolean hasEntries = false;

        for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
            String projectDirName = Preferences.getProperty("recentproject." + i + ".path");
            String projectName = Preferences.getProperty("recentproject." + i + ".name");

            if (projectDirName != null) {
                final File projectDir = new File(projectDirName);

                if (projectDir.exists()) {
                    hasEntries = true;

                    JMenuItem recentFileMenuItem = new JMenuItem(projectName);

                    recentFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(Preferences.sDYNAMIC_KEYS.get(i),
                            Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
                    recentFileMenuItem.addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent e) {
                            mEditorInstance.openProject(projectDir.getPath());
                        }
                    });
                    mOpenRecentFileMenu.add(recentFileMenuItem);
                }
            }
        }

        mOpenRecentFileMenu.setEnabled(hasEntries);

        if (hasEntries) {
            mOpenRecentFileMenu.add(new JSeparator());
            mOpenRecentFileMenu.add(mClearRecentFileMenuItem);
        }
    }

    private void initFileMenu() {
        mFileMenu = new JMenu("File");
        mCreateFileMenuItem = new JMenuItem("New Project...");

        // mCreateFileMenuItem.setIcon(new ImageIcon("data/img/new.png"));
        mCreateFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCreateFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                new NewProjectDialog();
            }
        });
        mOpenFileMenuItem = new JMenuItem("Open Project...");

        // mOpenFileMenuItem.setIcon(new ImageIcon("data/img/open.png"));
        mOpenFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mOpenFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.openProject();
                Preferences.save();
                refreshRecentFileMenu();
            }
        });
        mOpenRecentFileMenu = new JMenu("Open Recent Project");

        // mOpenRecentFileMenu.setIcon(new ImageIcon("data/img/recent.png"));
        mClearRecentFileMenuItem = new JMenuItem("Clear List");
        mClearRecentFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
                    String projectDirName = Preferences.getProperty("recentproject." + i + ".path");
                    String projectName = Preferences.getProperty("recentproject." + i + ".name");

                    if (projectDirName != null) {
                        Preferences.removeProperty("recentproject." + i + ".path");
                        Preferences.removeProperty("recentproject." + i + ".date");
                        Preferences.removeProperty("recentproject." + i + ".name");
                    }
                }
                mEditorInstance.clearRecentProjects();
                Preferences.save();
                refreshRecentFileMenu();
            }
        });
        refreshRecentFileMenu();

        mCloseFileMenuItem = new JMenuItem("Close Project");
//      mCloseFileMenuItem.setIcon(new ImageIcon("data/img/close.png"));
        mCloseFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCloseFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.close(QuitDialog.CLOSE_PROJ_DIALOG);
            }
        });

        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        ////////////////////////////////////////////////////////////////////////
        mSaveFileMenuItem = new JMenuItem("Save");
        // mSaveFileMenuItem.setIcon(new ImageIcon("data/img/save.png"));
        mSaveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mSaveFileMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // Save the current editor project
                mEditorInstance.save();
            }
        });
        mSaveAsMenuItem = new JMenuItem("Save As");
        mSaveAsMenuItem.setIcon(new ImageIcon("data/img/saveas.png"));
        mSaveAsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                (java.awt.event.InputEvent.SHIFT_MASK | (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()))));
        mSaveAsMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.saveAs();
            }

            // TODO Check if Projekt is new Project, then update recent file menu list.
        });
        mSaveAllMenuItem = new JMenuItem("Save All");
        mSaveAllMenuItem.setIcon(new ImageIcon("data/img/saveall.png"));
        mSaveAllMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
                (java.awt.event.InputEvent.ALT_MASK | (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()))));
        mSaveAllMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.saveAll();
            }
        });
        mExitEditorMenuItem = new JMenuItem("Quit");

//      mExitEditorMenuItem.setIcon(new ImageIcon("data/img/exit.png"));
        mExitEditorMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mExitEditorMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                // Close all project editors
                mEditorInstance.closeAll();
                // And finally exit the system
                //System.exit(0);
            }
        });
        mFileMenu.add(mCreateFileMenuItem);
        mFileMenu.add(mOpenFileMenuItem);
        mFileMenu.add(mOpenRecentFileMenu);
        mFileMenu.add(new JSeparator());
        mFileMenu.add(mCloseFileMenuItem);
        mFileMenu.add(mSaveFileMenuItem);
        mFileMenu.add(mSaveAsMenuItem);
        mFileMenu.add(mSaveAllMenuItem);

        if (System.getProperty("os.name").toLowerCase().indexOf("windows") != -1) {
            mFileMenu.add(new JSeparator());
            mFileMenu.add(mExitEditorMenuItem);
        }

        add(mFileMenu);
    }

    private void initEditMenu() {
        mEditMenu = new JMenu("Edit");
        
        //COPY ACTION
        mCopyMenuItem = new JMenuItem("Copy");
        mCopyMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCopyMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().copyNodes();
            }
        });
        //TODO CUT ACTION NOT IMPLEMENTED
        mCutMenuItem = new JMenuItem("Cut");
        mCutMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCutMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().cutNodes();
            }
        });
        //TODO PASTE ACTION NOT IMPLEMENTED
        mPasteMenuItem = new JMenuItem("Paste");
        mPasteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mPasteMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().pasteNodes();
            }
        });
        //TODO DELETE ACTIONS
        mDeleteMenuItem = new JMenuItem("Delete");
        mDeleteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        
        //NORMALIZE EDGES
        mNormalizeMenuItem = new JMenuItem("Normalize all Edges");
        mNormalizeMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N,
                (java.awt.event.InputEvent.ALT_MASK | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mNormalizeMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().normalizeAllEdges();
            }
        });
        //STRAIGHTEN EDGES
        mStraightenMenuItem = new JMenuItem("Straighen all Edges");
        mStraightenMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B,
                (java.awt.event.InputEvent.ALT_MASK | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mStraightenMenuItem.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().straightenAllEdges();
            }
        });
        
        //***************************************OPTIONS********************************************************************
        mOptionsMenuItem = new JMenuItem("Options");
        mOptionsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_COMMA,
                (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mOptionsMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.showOptions();
            }
        });

        Action undoAction = UndoAction.getInstance();

//      undoAction.putValue(Action.SMALL_ICON, new ImageIcon("data/img/undo.png"));
        Action redoAction = RedoAction.getInstance();

//      redoAction.putValue(Action.SMALL_ICON, new ImageIcon("data/img/redo.png"));
        mEditMenu.add(undoAction);
        mEditMenu.add(redoAction);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mCopyMenuItem);
        mEditMenu.add(mCutMenuItem);
        mEditMenu.add(mPasteMenuItem);
        mEditMenu.add(mDeleteMenuItem);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mStraightenMenuItem);
        mEditMenu.add(mNormalizeMenuItem);
        mEditMenu.add(new JSeparator());

//      mEditMenu.add(mFormatSceneDocument);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mOptionsMenuItem);
        add(mEditMenu);
    }

    private void initHelpMenu() {
        mHelpMenu = new JMenu("Help");
        mQuestionMenuItem = new JMenuItem("Help");

        // mQuestionMenuItem.setIcon(new ImageIcon("data/img/help.png"));
        mQuestionMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mQuestionMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.showHelp();
            }
        });
        mInfoMenuItem = new JMenuItem("About");

        // mInfoMenuItem.setIcon(new ImageIcon("data/img/about.png"));
        mInfoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I,
                Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mInfoMenuItem.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mEditorInstance.showAbout();
            }
        });
        mHelpMenu.add(mQuestionMenuItem);
        mHelpMenu.add(new JSeparator());
        mHelpMenu.add(mInfoMenuItem);
        add(mHelpMenu);
    }
}
