package de.dfki.vsm.editor;

import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.color.ColorSpace;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.awt.image.ColorConvertOp;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Observable;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;

/**
 *
 * @author mfallas Class implements welcome screen with a list of recent
 * projects
 */
public class WelcomePanel extends JPanel implements Observer {

//    private final JButton mOpenProjButton;
//    private final JButton mNewProjButton;
    private final Editor parentEditor;
    private final Box mRecentProjects;
    private final String backgroundImage = "/res/img/icon_big.png"; //Background for the welcome screen
    private final int paddingSize;
    private final Dimension screenDimension;
    private final Dimension buttonSize;
    private final Dimension halfScreenDimension;

    public WelcomePanel(final Editor mParent) {

        try {

            //UIManager.setLookAndFeel("javax.swing.plaf.nimbus.NimbusLookAndFeel");
        } catch (Exception e) {
        }
        parentEditor = mParent;
        buttonSize = new Dimension(100, 125);
        screenDimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        halfScreenDimension = new Dimension((int) (java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight() / 2), (int) java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight());
        paddingSize = (int) (0.075 * screenDimension.getHeight());
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createEmptyBorder(paddingSize, paddingSize, paddingSize, paddingSize));
        JLabel titleLabel = new JLabel("Welcome to Visual SceneMaker");
        titleLabel.setOpaque(false);
        titleLabel.setFont(new Font("Helvetica", Font.BOLD, 24));

        JLabel msgLabel = new JLabel("<html>This welcome screen provides quick starting actions, like a new project, open a recent project, <br>"
                + "open a example project, and check news and documentation</html>");
        msgLabel.setOpaque(false);
        msgLabel.setMaximumSize(new Dimension((int) (screenDimension.getWidth() / 2), 30));
        msgLabel.setFont(new Font("Helvetica", Font.PLAIN, 18));
        msgLabel.setBorder(BorderFactory.createEmptyBorder(10, 0, 30, 0));
        mRecentProjects = Box.createVerticalBox();
        mRecentProjects.setOpaque(false);
        //mRecentProjects.setMinimumSize(halfScreenDimension);
        mRecentProjects.setMaximumSize(halfScreenDimension);
        createListOfRecentProj();
        add(titleLabel);
        add(msgLabel);
//        JSeparator js = new JSeparator(JSeparator.HORIZONTAL);
//        js.setMaximumSize(new Dimension(5000, 1));
//        add(js);
        add(mRecentProjects);
        setMaximumSize(parentEditor.getSize());
        setPreferredSize(parentEditor.getSize());
        setOpaque(true);
        //setBackground(new Color(250, 250, 250));
        setBackground(Color.white);
    }

    @Override
    public void update(Observable o, Object o1) {

    }

    /**
     * Draws the image on the background of the welcome
     *
     * @param g
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Image image = ResourceLoader.loadImageIcon(backgroundImage).getImage();
//        ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
//        ColorConvertOp op = new ColorConvertOp(cs, null);
//
//        BufferedImage bufferedImage = new BufferedImage(img.getWidth(null), img.getHeight(null),
//                BufferedImage.TYPE_INT_ARGB);
//        //Graphics g2 = bufferedImage.createGraphics();
//
//        Graphics2D g2d = (Graphics2D) bufferedImage.getGraphics();
//        g2d.setComposite(AlphaComposite.SrcOver.derive(0.1f));
//        g2d.drawImage(img, 0, 0, null);
//        g2d.dispose();
//        BufferedImage image = op.filter(bufferedImage, null);
        //g.drawImage(ResourceLoader.loadImageIcon("/res/img/welcome_message.png").getImage(), getWidth() / 3, paddingSize / 2, this);
        g.drawImage(image, -300, 0, null);
    }

    /**
     * Creates the list of recent projects
     */
    public void createListOfRecentProj() {

        mRecentProjects.removeAll();
        //PROJECTS SECTION
        JLabel actionMenu = new JLabel("Projects");
        actionMenu.setMaximumSize(new Dimension(5000, 75));
        actionMenu.setPreferredSize(new Dimension(500, 50));
        actionMenu.setOpaque(true);
        actionMenu.setBackground(new Color(255, 255, 255, 180));
        actionMenu.setFont(new Font("Helvetica", Font.PLAIN, 24));
        mRecentProjects.add(actionMenu);
        //NEW PROJECT BUTTON
        JLabel mNewProjMenu = new JLabel("New Project");
        mNewProjMenu.setToolTipText("Create New Project");
        mNewProjMenu.setIcon(ResourceLoader.loadImageIcon("/res/img/arrow_icon.png"));
        mNewProjMenu.setMaximumSize(new Dimension(5000, 75));
        mNewProjMenu.setPreferredSize(new Dimension(500, 50));
        mNewProjMenu.setFont(new Font("Helvetica", Font.PLAIN, 18));
        mNewProjMenu.setOpaque(false);
        mNewProjMenu.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                parentEditor.newProject();
            }

            public void mouseEntered(MouseEvent me) {
                ((JLabel) me.getComponent()).setOpaque(true);
                me.getComponent().setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                ((JLabel) me.getComponent()).setOpaque(false);
                me.getComponent().setBackground(new Color(1f, 1f, 1f));

            }
        });
        mRecentProjects.add(mNewProjMenu);
        JLabel mOpenProjectMenu = new JLabel("Open a Project");
        mOpenProjectMenu.setToolTipText("Open an external Project");
        mOpenProjectMenu.setIcon(ResourceLoader.loadImageIcon("/res/img/arrow_icon.png"));
        mOpenProjectMenu.setMaximumSize(new Dimension(5000, 75));
        mOpenProjectMenu.setPreferredSize(new Dimension(500, 50));
        mOpenProjectMenu.setOpaque(false);
        mOpenProjectMenu.setFont(new Font("Helvetica", Font.PLAIN, 18));
        mOpenProjectMenu.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                parentEditor.openProject();
            }

            public void mouseEntered(MouseEvent me) {
                ((JLabel) me.getComponent()).setOpaque(true);
                me.getComponent().setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                ((JLabel) me.getComponent()).setOpaque(false);
                me.getComponent().setBackground(new Color(255, 255, 255, 180));
            }
        });
        mRecentProjects.add(mOpenProjectMenu);
        JSeparator jsNP = new JSeparator();
        jsNP.setMaximumSize(new Dimension(5000, 10));
        mRecentProjects.add(jsNP);
        JLabel titleMenu = new JLabel("Open Recent Project");
        titleMenu.setBorder(null);
        titleMenu.setOpaque(true);

        titleMenu.setBackground(new Color(255, 255, 255, 180));
        titleMenu.setFont(new Font("Helvetica", Font.PLAIN, 24));
        mRecentProjects.add(titleMenu);
        titleMenu.setMaximumSize(new Dimension(5000, 75));
        titleMenu.setPreferredSize(new Dimension(500, 50));
        SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
        JLabel[] projectList = new JLabel[Preferences.sMAX_RECENT_FILE_COUNT];
        JPanel recentPanel = new JPanel();
        recentPanel.setOpaque(false);
        recentPanel.setLayout(new BoxLayout(recentPanel, BoxLayout.Y_AXIS));
        for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
            String projectDirName = Preferences.getProperty("recentprojectdir" + i);
            String projectName = Preferences.getProperty("recentprojectname" + i);
            if (projectDirName != null) {
                final File projectDir = new File(projectDirName);
                if (projectDir.exists()) {
                    projectList[i] = new JLabel(projectName + ", last edited: " + sdf.format(projectDir.lastModified()));
                    projectList[i].setLayout(new BoxLayout(projectList[i], BoxLayout.X_AXIS));
                    projectList[i].setOpaque(false);
                    projectList[i].setMaximumSize(new Dimension(5000, 75));
                    projectList[i].setPreferredSize(new Dimension(5000, 50));
                    projectList[i].setFont(new Font("Helvetica", Font.PLAIN, 18));
                    projectList[i].setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
                    projectList[i].addMouseListener(new MouseListener() {

                        @Override
                        public void mouseClicked(MouseEvent me) {
                            parentEditor.toggleProjectEditorList(true);
                            parentEditor.openProject(projectDir);
                        }

                        @Override
                        public void mousePressed(MouseEvent me) {
                        }

                        @Override
                        public void mouseReleased(MouseEvent me) {
                        }

                        @Override
                        public void mouseEntered(MouseEvent me) {
                            ((JLabel) me.getComponent()).setOpaque(true);
                            me.getComponent().setBackground(new Color(82, 127, 255));
                        }

                        @Override
                        public void mouseExited(MouseEvent me) {
                            ((JLabel) me.getComponent()).setOpaque(false);
                            me.getComponent().setBackground(new Color(1f, 1f, 1f));

                        }
                    });
                    recentPanel.add(projectList[i]);
                    JSeparator js = new JSeparator();
                    js.setMaximumSize(new Dimension(5000, 1));
                    recentPanel.add(js);
                }
            }
        }
        recentPanel.remove(recentPanel.getComponentCount() - 1);
        JScrollPane jsRecent = new JScrollPane(recentPanel);
        jsRecent.setOpaque(false);
        jsRecent.setMaximumSize(new Dimension(5000, 75));
        jsRecent.setPreferredSize(new Dimension(500, 50));
        jsRecent.setBorder(null);
        jsRecent.getViewport().setOpaque(false);
        jsRecent.setMaximumSize(halfScreenDimension);
        mRecentProjects.add(recentPanel);
        JSeparator jsSP = new JSeparator();
        jsNP.setMaximumSize(new Dimension(5000, 1));
        mRecentProjects.add(jsSP);
        JLabel exampleMenu = new JLabel("Sample Projects");
        exampleMenu.setBorder(null);
        exampleMenu.setMaximumSize(new Dimension(5000, 75));
        exampleMenu.setPreferredSize(new Dimension(500, 50));
        exampleMenu.setOpaque(true);
        exampleMenu.setBackground(new Color(255, 255, 255, 180));
        exampleMenu.setFont(new Font("Helvetica", Font.PLAIN, 24));
        mRecentProjects.add(exampleMenu);
        JLabel mDocuMenu = new JLabel("News and Documentation");
        mDocuMenu.setToolTipText("News and Documentation online");
        mDocuMenu.setMaximumSize(new Dimension(5000, 75));
        mDocuMenu.setPreferredSize(new Dimension(500, 50));
        mDocuMenu.setOpaque(true);
        mDocuMenu.setBackground(new Color(255, 255, 255, 180));
        mDocuMenu.setFont(new Font("Helvetica", Font.PLAIN, 24));
        mRecentProjects.add(mDocuMenu);
        JLabelURL link = new JLabelURL("Visual SceneMaker Online", "http://scenemaker.dfki.de/");

        mRecentProjects.add(link);
    }

}
