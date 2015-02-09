/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.editor;

import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Label;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

/**
 *
 * @author mfallas Class implements welcome screen with a list of recent
 * projects
 */
public class WelcomePanel extends JPanel implements Observer {

    private JButton mOpenProjButton;
    private JButton mNewProjButton;
    private JList mRecentProjects;
    private DefaultListModel model;
    private Editor parentEditor;

    public WelcomePanel(final Editor mParent) {
        parentEditor = mParent;
        setBackground(java.awt.Color.white);
        setLayout(new GridLayout(2,2));
        setBorder(BorderFactory.createEmptyBorder(100, 100, 100, 100));
        
        /**
         * ELEMENTS
         */
        JPanel container = new JPanel(new FlowLayout(FlowLayout.LEFT));
        container.setBackground(Color.white);
        container.setBorder(BorderFactory.createTitledBorder("Recent Projects"));
        mOpenProjButton = new JButton(ResourceLoader.loadImageIcon("/res/img/openproject.png"));
        mOpenProjButton.setFocusable(false);
        mOpenProjButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mParent.openProject();
            }
        });
        mNewProjButton = new JButton(ResourceLoader.loadImageIcon("/res/img/newproject.png"));
        mNewProjButton.setFocusable(false);
        mNewProjButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                mParent.newProject();
            }
        });
        JMenuBar mRecentProjectsBar = new JMenuBar();
        mRecentProjectsBar.setBackground(Color.white);
        mRecentProjectsBar.setLayout(new BoxLayout(mRecentProjectsBar, BoxLayout.PAGE_AXIS));
        mRecentProjectsBar.setBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.BLACK));
        for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
            String projectDirName = Preferences.getProperty("recentprojectdir" + i);
            String projectName = Preferences.getProperty("recentprojectname" + i);
            if (projectDirName != null) {
                final File projectDir = new File(projectDirName);
                if (projectDir.exists()) {
                    JMenuItem recentFileMenuItem = new JMenuItem(projectName);
                    recentFileMenuItem.setBackground(Color.WHITE);
                    recentFileMenuItem.setOpaque(true);
                    recentFileMenuItem.setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
                    recentFileMenuItem.addMouseListener(new MouseListener() {

                        @Override
                        public void mouseClicked(MouseEvent me) {
                            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
                        }

                        @Override
                        public void mousePressed(MouseEvent me) {
                            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
                        }

                        @Override
                        public void mouseReleased(MouseEvent me) {
                            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
                        }

                        @Override
                        public void mouseEntered(MouseEvent me) {
                            me.getComponent().setBackground(new Color(82, 127, 255)); 
                        }

                        @Override
                        public void mouseExited(MouseEvent me) {
                            me.getComponent().setBackground(Color.white);
                        }
                    });
                    recentFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(Preferences.sDYNAMIC_KEYS.get(i), Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
                    recentFileMenuItem.addActionListener(new ActionListener() {

                        public void actionPerformed(ActionEvent e) {
                            mParent.toggleProjectEditorList(true);
                            parentEditor.openProject(projectDir);
                        }
                    });
                    mRecentProjectsBar.add(recentFileMenuItem);
                }
            }
        }
        container.add(mRecentProjectsBar);
        add(container);
        Box bx = Box.createHorizontalBox();
        bx.add(mNewProjButton);
        bx.add(mOpenProjButton);
        add(bx);

    }

    @Override
    public void update(Observable o, Object o1) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
