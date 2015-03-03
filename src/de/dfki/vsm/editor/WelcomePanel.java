package de.dfki.vsm.editor;

import de.dfki.vsm.editor.util.Preferences;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.color.ColorSpace;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.KeyStroke;
import javax.swing.UIManager;

/**
 *
 * @author mfallas Class implements welcome screen with a list of recent
 * projects
 */
public class WelcomePanel extends JPanel implements Observer {

    private final JButton mOpenProjButton;
    private final JButton mNewProjButton;
    private Editor parentEditor;
    private final JMenuBar mRecentProjectsBar;
    private final String backgroundImage = "/res/img/icon_big.png"; //Background for the welcome screen
    private final int paddingSize;
    private final Dimension screenDimension;

    public WelcomePanel(final Editor mParent) {

        parentEditor = mParent;
        screenDimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        paddingSize = (int) (0.075 * screenDimension.getHeight());
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorder(BorderFactory.createEmptyBorder(paddingSize * 2, paddingSize, paddingSize, paddingSize));
        //LEFT AND RIGHT PANELS
        Box leftContainer = Box.createVerticalBox();
        Box righContainer = Box.createVerticalBox();

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////LEFT SIDE ELEMENTS
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //CONTAINER FOR THE RECENT PROJECTS
        Box bxRecentProj = Box.createVerticalBox();
        bxRecentProj.setOpaque(false);
        bxRecentProj.setBorder(BorderFactory.createTitledBorder("Recent Projects"));
        //bxRecentProj.setMaximumSize(new Dimension((int)(screenDimension.getWidth()/3), (int)screenDimension.getHeight()));
        //OPEN PROJECT BUTTON
        mOpenProjButton = new JButton(ResourceLoader.loadImageIcon("/res/img/open_project_icon.png"));
        mOpenProjButton.setRolloverEnabled(true);
        mOpenProjButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/open_project_icon_blue.png"));
        mOpenProjButton.setToolTipText("Open a Project");
        mOpenProjButton.setFocusable(false);
        mOpenProjButton.setBorder(null);
        mOpenProjButton.setContentAreaFilled(false);
        mOpenProjButton.setOpaque(false);
        mOpenProjButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                parentEditor.openProject();
            }
        });

        //NEW PROJECT BUTTON
        mNewProjButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new_project_icon.png"));
        mNewProjButton.setRolloverEnabled(true);
        mNewProjButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/new_project_icon_blue.png"));
        mNewProjButton.setToolTipText("Create New Project");
        mNewProjButton.setFocusable(false);
        mNewProjButton.setBorder(null);
        mNewProjButton.setContentAreaFilled(false);
        mNewProjButton.setOpaque(false);
        mNewProjButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                parentEditor.newProject();
            }
        });
        //RECENTPROJECTS PANEL
        mRecentProjectsBar = new JMenuBar();
        mRecentProjectsBar.setLayout(new BoxLayout(mRecentProjectsBar, BoxLayout.PAGE_AXIS));
        mRecentProjectsBar.setOpaque(false);
        mRecentProjectsBar.setBackground(Color.white);
        mRecentProjectsBar.setBorder(null);
        UIManager.put("MenuItem.opaque", false);
        createListOfRecentProj();
        bxRecentProj.add(mRecentProjectsBar);
        //BUTTONS CONTAINER
        Box bxButtons = Box.createHorizontalBox();
        bxButtons.setBorder(BorderFactory.createEmptyBorder(paddingSize, 0, 0, 0));
        bxButtons.add(mNewProjButton);
        JPanel empty = new JPanel();
        empty.setOpaque(false);
        empty.setMaximumSize(new Dimension(paddingSize, 10));
        bxButtons.add(empty);
        bxButtons.add(mOpenProjButton);
        bxButtons.setAlignmentX(LEFT_ALIGNMENT);
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////MIDDLE ELEMENTS
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        String infoMessage = "Lorem ipsum dolor sit amet, eu albucius euripidis reprehendunt vel, no nam case eros argumentum. Qui agam torquatos cu, ut alii constituto usu, eos nullam altera ut. Vix tota vivendo eu, his oporteat iudicabit prodesset an, conceptam reformidans ut pro. Libris rationibus ne nec, timeam delectus pro et. Mel cu inani virtute, putent inimicus et ius.\n"
                + "\n"
                + "Ut debitis similique eum, tempor albucius recteque te quo. Latine appellantur ex per, apeirian disputando dissentiet nam ei. Pri iusto quaerendum ea, nec voluptatum voluptatibus ei. Hinc quot oblique nec eu, ei quo dolor virtute debitis, ne his vero lobortis.\n"
                + "\n"
                + "No pro dolorum pericula contentiones, in qui mundi efficiendi. Quo alia duis graece ex, putent ullamcorper eos cu, an sea dicant quaeque. Facilisis accusamus argumentum ea quo, quot alienum invidunt ex nam. Id forensibus deseruisse his. Vim et laoreet voluptatum.\n"
                + "\n"
                + "Ius putent graeco suscipit ea. Quo an erat fugit verterem, quem inciderint persequeris ut mel. Eam inermis apeirian in, eam in labores fabellas. Vix te case eius nominavi, eum ad viris diceret, ei mazim minimum recusabo eos.\n"
                + "\n"
                + "Modo vivendo ad usu, esse animal nam id. Sonet postulant omittantur sed id, te amet docendi consulatu his. Aliquip sadipscing pri an, at fastidii facilisi accommodare has, et ullum rationibus sed. Illud explicari interpretaris eam ex, usu no noluisse probatus oportere. Persequeris philosophia ne pro, sonet quando incorrupte duo ut. In liber ridens delicata nec, quo et phaedrum necessitatibus, an erant iisque has.\n"
                + "\n"
                + "Copiosae praesent id has, te quo congue expetendis. Summo molestie rationibus ut per, mea quas aperiam ea. Quo et utinam quaeque. Ex torquatos moderatius delicatissimi has. Ut vis novum tritani torquatos, in voluptua maluisset repudiare per, eum semper bonorum ad.\n"
                + "\n"
                + "Id his nibh habeo tempor, cu sit tollit ocurreret. Sit ad feugait principes sententiae, discere inimicus eam in. His ex velit graecis probatus, ei modo ridens vis. Option epicurei id mel, cu vix salutatus rationibus. Ut vim tota patrioque. Ut sit efficiendi adversarium, ex eos iusto mollis imperdiet. Eam vidit everti ut.\n"
                + "\n"
                + "Vis aeque vocent nominati te, ex consequat torquatos quo, id vis mutat fugit. No malis ridens malorum per, modo iracundia cu ius. Cu sed esse natum dissentiunt, similique interpretaris no usu. Ex eos alterum conceptam. No diceret postulant mei, tantas verterem sensibus his an.\n"
                + "\n"
                + "Primis graecis has in, ubique scripserit id his. Quot electram usu ex, cum delicata accusamus expetendis in. Eam et nisl tempor deterruisset, epicuri perfecto scripserit vis at. Et hinc autem senserit his. Sea alterum voluptatum ut. Purto laudem soleat ex usu, ex pro stet viris.";

        JEditorPane infoArea = new JEditorPane();
        infoArea.setEditable(false);
        infoArea.setText(infoMessage);
        infoArea.setOpaque(false);

        /*JScrollPane textScroll = new JScrollPane(infoArea);
         textScroll.setOpaque(false);*/
        //infoArea.setBackground(new Color(0, 0, 0, 0.0f));
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////RIGHT SIDE ELEMENTS
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        JPanel sampleProjects = new JPanel();
        sampleProjects.setBorder(BorderFactory.createTitledBorder("Sample Projects"));
        sampleProjects.setOpaque(false);
        //sampleProjects.setMinimumSize(new Dimension(600, 600));
        JPanel helpPanel = new JPanel();
        helpPanel.setBorder(BorderFactory.createTitledBorder("Help and Tutorials"));
        helpPanel.setOpaque(false);
        //helpPanel.setMinimumSize(new Dimension(600, 600));

        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        /////////////////////////////////////////////////////////SCREEN SETUP
        ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        //SETUP LEFT SIDE
        leftContainer.add(bxRecentProj);
        leftContainer.add(bxButtons);

        //SETUP RIGHT SIDE
        righContainer.setBorder(BorderFactory.createEmptyBorder(0, paddingSize, paddingSize, 0));
        righContainer.add(sampleProjects);
        righContainer.add(helpPanel);
        //SETUP THE SCREEN
        add(new JSeparator(JSeparator.HORIZONTAL));
        JPanel elementContainer = new JPanel(new GridLayout(0, 3));
        elementContainer.setOpaque(false);
        elementContainer.add(leftContainer);
        elementContainer.add(infoArea);
        elementContainer.add(righContainer);
        add(elementContainer);
        add(new JSeparator(JSeparator.HORIZONTAL));
        setOpaque(true);
        setBackground(Color.white);
    }

    @Override
    public void update(Observable o, Object o1) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * Draws the image on the background of the welcome
     *
     * @param g
     */
    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Image img = ResourceLoader.loadImageIcon(backgroundImage).getImage();
        ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_GRAY);
        ColorConvertOp op = new ColorConvertOp(cs, null);

        BufferedImage bufferedImage = new BufferedImage(img.getWidth(null), img.getHeight(null),
                BufferedImage.TYPE_INT_ARGB);
        //Graphics g2 = bufferedImage.createGraphics();
        Graphics2D g2d = (Graphics2D) bufferedImage.getGraphics();
        g2d.setComposite(AlphaComposite.SrcOver.derive(0.1f));
        g2d.drawImage(img, 0, 0, null);
        g2d.dispose();
        //BufferedImage image = op.filter(bufferedImage, null);
        g.drawImage(ResourceLoader.loadImageIcon("/res/img/welcome_message.png").getImage(), getWidth() / 3, paddingSize / 2, this);
        g.drawImage(bufferedImage, getWidth() / 4, paddingSize, null);
    }

    /**
     * Creates the list of recent projects
     */
    public void createListOfRecentProj() {
        mRecentProjectsBar.removeAll();
        SimpleDateFormat sdf = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        for (int i = 0; i <= Preferences.sMAX_RECENT_FILE_COUNT; i++) {
            String projectDirName = Preferences.getProperty("recentprojectdir" + i);
            String projectName = Preferences.getProperty("recentprojectname" + i);
            if (projectDirName != null) {
                final File projectDir = new File(projectDirName);
                if (projectDir.exists()) {
                    JMenuItem recentFileMenuItem = new JMenuItem(projectName + "  (" + sdf.format(projectDir.lastModified()) + ")");
                    recentFileMenuItem.setBorder(null);
                    recentFileMenuItem.setContentAreaFilled(false);
                    recentFileMenuItem.setOpaque(true);
                    recentFileMenuItem.setBackground(Color.white);
                    recentFileMenuItem.setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
                    recentFileMenuItem.addMouseListener(new MouseListener() {
                        @Override
                        public void mouseClicked(MouseEvent me) {
                        }

                        @Override
                        public void mousePressed(MouseEvent me) {
                        }

                        @Override
                        public void mouseReleased(MouseEvent me) {
                        }

                        @Override
                        public void mouseEntered(MouseEvent me) {
                            //recentFileMenuItem.setOpaque(true);
                            me.getComponent().setBackground(new Color(82, 127, 255));
                        }

                        @Override
                        public void mouseExited(MouseEvent me) {
                            //recentFileMenuItem.setOpaque(false);
                            me.getComponent().setBackground(Color.white);
                        }
                    });
                    recentFileMenuItem.setAccelerator(KeyStroke.getKeyStroke(Preferences.sDYNAMIC_KEYS.get(i), Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
                    recentFileMenuItem.addActionListener(new ActionListener() {

                        public void actionPerformed(ActionEvent e) {
                            //parentEditor.toggleProjectEditorList(true);
                            parentEditor.openProject(projectDir);
                        }
                    });
                    mRecentProjectsBar.add(recentFileMenuItem);
                }
            }
        }
    }

}
