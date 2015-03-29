package de.dfki.vsm.editor;

import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.util.evt.EventCaster;
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
 * @author Parick Gebhard
 */
public class MenuBar extends JMenuBar {

    private final Editor mEditor;
    // File menu
    private JMenu mFileMenu;
    private JMenuItem mCreateFileMenuItem;
    private JMenuItem mOpenFileMenuItem;
    private JMenuItem mOpenSceneflowMenuItem;
    private JMenuItem mOpenRecentFileMenu;
    private JMenuItem mClearRecentFileMenuItem;
    private JMenuItem mSaveFileMenuItem;
//  private JMenuItem mSaveFileAsMenuItem;
    private JMenuItem mSaveAllMenuItem;
    private JMenuItem mCloseFileMenuItem;
    private JMenuItem mExitEditorMenuItem;
    // Edit menu
    private JMenu mEditMenu;
    private JMenuItem mCutMenuItem;
    private JMenuItem mCopyMenuItem;
    private JMenuItem mPasteMenuItem;
    private JMenuItem mDeleteMenuItem;
    private JMenuItem mStraightenAllEdgesMenuItem;
    private JMenuItem mNormalizeAllEdgesMenuItem;
    private JMenuItem mOptionsMenuItem;
    // Help menu
    private JMenu mHelpMenu;
    private JMenuItem mQuestionMenuItem;
    private JMenuItem mInfoMenuItem;
    private JMenuItem mSaveFileAsMenuItem;

    public MenuBar(final Editor editor) {
        mEditor = editor;
        initComponents();
    }

//  public void setRunMenuEnabled(boolean flag) {
//    mRunMenuItem.setEnabled(flag);
//  }
//  public void setStopMenuEnabled(boolean flag) {
//    mStopMenuItem.setEnabled(flag);
//  }
//
//  public void setPauseMenuText(String value) {
//    mPauseMenuItem.setText(value);
//  }
//
//  public void setPauseMenuEnabled(boolean flag) {
//    mPauseMenuItem.setEnabled(flag);
//  }
    public void setCloseMenuEnabled(boolean flag) {
        mCloseFileMenuItem.setEnabled(flag);
    }

    public void setFileSaveMenuEnabled(boolean flag) {
        mSaveFileMenuItem.setEnabled(flag);
//    mSaveFileAsMenuItem.setEnabled(flag);
        mSaveAllMenuItem.setEnabled(flag);
    }
//
//  public void setMonitorMenuEnabled(boolean flag) {
//    mMonitorMenuItem.setEnabled(flag);
//  }

//     public void setMonitorMenuEnabled(boolean flag) {
//        mMonitorMenuItem.setEnabled(flag);
//    }
//
//      public void setMonitorMenuEnabled(boolean flag) {
//        mMonitorMenuItem.setEnabled(flag);
//    }
//
//
//       public void setMonitorMenuEnabled(boolean flag) {
//        mMonitorMenuItem.setEnabled(flag);
//    }
//
//        public void setMonitorMenuEnabled(boolean flag) {
//        mMonitorMenuItem.setEnabled(flag);
//    }
    private void initComponents() {
        initFileMenu();
        initEditMenu();
        initHelpMenu();
    }

    private void refreshRecentFileMenu() {
        mOpenRecentFileMenu.removeAll();

        boolean hasEntries = false;
        for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
            String projectDirName = Preferences.getProperty("recentprojectdir" + i);
            String projectName = Preferences.getProperty("recentprojectname" + i);

            if (projectDirName != null) {
                final File projectDir = new File(projectDirName);
                if (projectDir.exists()) 
                {
                    hasEntries = true;
                    JMenuItem recentFileMenuItem = new JMenuItem(projectName);
                    recentFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(Preferences.sDYNAMIC_KEYS.get(i), Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
                    recentFileMenuItem.addActionListener(new ActionListener() {

                        public void actionPerformed(ActionEvent e) {
                            mEditor.openProject(projectDir);
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
        //mCreateFileMenuItem.setIcon(new ImageIcon("data/img/new.png"));
        mCreateFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCreateFileMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.newProject();
            }
        });
        mOpenFileMenuItem = new JMenuItem("Open Project...");
        //mOpenFileMenuItem.setIcon(new ImageIcon("data/img/open.png"));
        mOpenFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mOpenFileMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.openProject();

                Preferences.save();

                refreshRecentFileMenu();
            }
        });

        mOpenRecentFileMenu = new JMenu("Open Recent Project");
        //mOpenRecentFileMenu.setIcon(new ImageIcon("data/img/recent.png"));
        mClearRecentFileMenuItem = new JMenuItem("Clear List");

        mClearRecentFileMenuItem.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
                    String projectDirName = Preferences.getProperty("recentprojectdir" + i);
                    String projectName = Preferences.getProperty("recentprojectname" + i);
                    if (projectDirName != null) {
                        Preferences.removeProperty("recentprojectdir" + i);
                        Preferences.removeProperty("recentprojectname" + i);
                    }
                }
                
                Preferences.save();

                refreshRecentFileMenu();
            }
        });

        refreshRecentFileMenu();

        mOpenSceneflowMenuItem = new JMenuItem("Open Sceneflow...");
        //mOpeSceneflowMenuItem.setIcon(new ImageIcon("data/img/open.png"));
        mOpenSceneflowMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mOpenSceneflowMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.openProject();
            }
        });

        mCloseFileMenuItem = new JMenuItem("Close");
//      mCloseFileMenuItem.setIcon(new ImageIcon("data/img/close.png"));
        mCloseFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCloseFileMenuItem.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                mEditor.closeCurrentProject();
            }
        });

        mSaveFileMenuItem = new JMenuItem("Save");
  //    mSaveFileMenuItem.setIcon(new ImageIcon("data/img/save.png"));
        mSaveFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mSaveFileMenuItem.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                ProjectData p = mEditor.saveCurrentProject();
                if (p.isPending()) {
                    p.setPending(false);
                    refreshRecentFileMenu();
                }
            }
        });

        mSaveFileAsMenuItem = new JMenuItem("Save As");
        mSaveFileAsMenuItem.setIcon(new ImageIcon("data/img/saveas.png"));
        mSaveFileAsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, (java.awt.event.InputEvent.SHIFT_MASK | (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()))));
        mSaveFileAsMenuItem.addActionListener(new ActionListener() {

          public void actionPerformed(ActionEvent e) {
            mEditor.saveFileAs();
          }

          // TODO Check if Projekt is new Project, then update recent file menu list.

        });
        
        mSaveAllMenuItem = new JMenuItem("Save All");
        mSaveAllMenuItem.setIcon(new ImageIcon("data/img/saveall.png"));
        mSaveAllMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, (java.awt.event.InputEvent.ALT_MASK | (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()))));
        mSaveAllMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.saveAllProjects();
            }
        });

        mExitEditorMenuItem = new JMenuItem("Quit");
//    mExitEditorMenuItem.setIcon(new ImageIcon("data/img/exit.png"));
        mExitEditorMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mExitEditorMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.exit();
            }
        });
        mFileMenu.add(mCreateFileMenuItem);
        mFileMenu.add(mOpenFileMenuItem);
        mFileMenu.add(mOpenRecentFileMenu);
        mFileMenu.add(new JSeparator());
        mFileMenu.add(mOpenSceneflowMenuItem);
        mFileMenu.add(new JSeparator());
        mFileMenu.add(mCloseFileMenuItem);
        mFileMenu.add(mSaveFileMenuItem);
        mFileMenu.add(mSaveFileAsMenuItem);
        mFileMenu.add(mSaveAllMenuItem);

        if (System.getProperty("os.name").toLowerCase().indexOf("windows") != -1) {
            mFileMenu.add(new JSeparator());
            mFileMenu.add(mExitEditorMenuItem);
        }

        add(mFileMenu);
    }

    private void initEditMenu() {
        mEditMenu = new JMenu("Edit");
        mCopyMenuItem = new JMenuItem("Copy");
//    mCopyMenuItem.setIcon(new ImageIcon("data/img/copy.png"));
        mCopyMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mCopyMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().copyNodes();
            }
        });
        mCutMenuItem = new JMenuItem("Cut");
//    mCutMenuItem.setIcon(new ImageIcon("data/img/cut.png"));
        mCutMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mPasteMenuItem = new JMenuItem("Paste");
//    mPasteMenuItem.setIcon(new ImageIcon("data/img/paste.png"));
        mPasteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mDeleteMenuItem = new JMenuItem("Delete");
//    mDeleteMenuItem.setIcon(new ImageIcon("data/img/delete.png"));
        mDeleteMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));//, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mNormalizeAllEdgesMenuItem = new JMenuItem("Normalize all Edges");
        mNormalizeAllEdgesMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, (java.awt.event.InputEvent.ALT_MASK | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mNormalizeAllEdgesMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().normalizeAllEdges();
            }
        });
        mStraightenAllEdgesMenuItem = new JMenuItem("Straighen all Edges");
        mStraightenAllEdgesMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_B, (java.awt.event.InputEvent.ALT_MASK | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mStraightenAllEdgesMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().straightenAllEdges();
            }
        });
//        mFormatSceneDocument = new JMenuItem("Format Scene Document");
//        mFormatSceneDocument.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, (java.awt.event.InputEvent.ALT_MASK | Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
//        mFormatSceneDocument.addActionListener(new ActionListener() {
//
//            public void actionPerformed(ActionEvent e) {
//                mEditor.getProjectEditorList().getSelectedProjectEditor().getSceneDocumentEditor().formatSceneDocument();
//            }
//        });
        mOptionsMenuItem = new JMenuItem("Options");
//    mOptionsMenuItem.setIcon(new ImageIcon("data/img/options.png"));
        mOptionsMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_COMMA, (Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())));
        mOptionsMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.showOptions();
            }
        });

        Action undoAction = UndoAction.getInstance();
//    undoAction.putValue(Action.SMALL_ICON, new ImageIcon("data/img/undo.png"));

        Action redoAction = RedoAction.getInstance();
//    redoAction.putValue(Action.SMALL_ICON, new ImageIcon("data/img/redo.png"));

        mEditMenu.add(undoAction);
        mEditMenu.add(redoAction);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mCopyMenuItem);
        mEditMenu.add(mCutMenuItem);
        mEditMenu.add(mPasteMenuItem);
        mEditMenu.add(mDeleteMenuItem);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mStraightenAllEdgesMenuItem);
        mEditMenu.add(mNormalizeAllEdgesMenuItem);
        mEditMenu.add(new JSeparator());
//        mEditMenu.add(mFormatSceneDocument);
        mEditMenu.add(new JSeparator());
        mEditMenu.add(mOptionsMenuItem);
        add(mEditMenu);
    }

    private void initHelpMenu() {
        mHelpMenu = new JMenu("Help");
        mQuestionMenuItem = new JMenuItem("Help");
        //mQuestionMenuItem.setIcon(new ImageIcon("data/img/help.png"));
        mQuestionMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_H, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mQuestionMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.showHelp();
            }
        });

        mInfoMenuItem = new JMenuItem("About");
        //mInfoMenuItem.setIcon(new ImageIcon("data/img/about.png"));
        mInfoMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
        mInfoMenuItem.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mEditor.showAbout();
            }
        });
        mHelpMenu.add(mQuestionMenuItem);
        mHelpMenu.add(new JSeparator());
        mHelpMenu.add(mInfoMenuItem);
        add(mHelpMenu);
    }
}
