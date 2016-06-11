package de.dfki.vsm.editor;

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.stickman.Stickman;
import de.dfki.vsm.editor.dialog.NewProjectDialog;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.SceneMaker3;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.xtesting.NewPropertyManager.PropertyManagerGUI;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Window;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import javax.swing.*;
import javax.swing.border.EmptyBorder;

/**
 *
 * @author mfallas, Patrick Gebhard
 *
 * Class implements welcome screen with link list of recent projects and sample
 * projects
 *
 */
public class EditorStarter extends JPanel {

	private final static Dimension screenDimension = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
	private final static Dimension halfScreenDimension = new Dimension((int) (java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight() / 2), (int) java.awt.Toolkit.getDefaultToolkit().getScreenSize().getHeight());
	private final static Dimension buttonSize = new Dimension((int) halfScreenDimension.getWidth(), 50);
	private final static int paddingSize = (int) (0.075 * screenDimension.getHeight());
	private final static Color sMENUHEADLINECOLOR = new Color(255, 255, 255, 182);
	private final static Color sMENUITEMBACKBGROUNDCOLOR = new Color(255, 255, 255, 128);
	private final static Color sHIGHLIGHTCOLOR = new Color(82, 127, 255, 182);
	private final static Color sTEXTCOLOR = new Color(16, 16, 16, 182);
	private final static Font sMENUHEADLINEFONT = new Font("Helvetica", Font.PLAIN, 24);
	private final static Font sMENUITEMFONT = new Font("Helvetica", Font.PLAIN, 18);
	// Welcome Stickman
	private final static Stickman mWelcomeStickman = new Stickman("", (Math.random() > 0.5) ? Stickman.TYPE.FEMALE : Stickman.TYPE.MALE, 1.5f);
	private boolean mShowStickman = true;
	
	private final File SampleProjFolder = new File(Preferences.sSAMPLE_PROJECTS);
	private final File TutorialsProjFolder = new File(Preferences.sTUTORIALS_PROJECTS);

	private final EditorInstance mEditorInstance;
	private final Box mCenterProjectBox;
	private final Box mLeftProjectBox;//Recent Projects
	private final Box mRightProjectBox;
	private JFrame mParentFrame;

	// The singelton logger instance
	private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

	private class CoolSeparator extends JSeparator {

		Dimension size = new Dimension(screenDimension.width, 1);

		public CoolSeparator() {
			setSize(size);
			setPreferredSize(size);
			setMaximumSize(size);
		}

		@Override
		public void paintComponent(Graphics g) {
			Graphics2D g2 = (Graphics2D) g;

			g2.setColor(sMENUHEADLINECOLOR);
			for (int x = 0; x < size.width; x += 10) {
				g2.drawLine(x, 0, x + 5, 0);
			}
		}
	}

	public EditorStarter(final EditorInstance mParent) {
		mEditorInstance = mParent;

		JPanel content = new JPanel();
		content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));
		content.setOpaque(false);

		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		setBorder(BorderFactory.createEmptyBorder(paddingSize, 180, paddingSize, paddingSize));

		JLabel titleLabel = new JLabel("Welcome to Visual SceneMaker");
		titleLabel.setOpaque(false);
		titleLabel.setMaximumSize(new Dimension((int) (screenDimension.getWidth() / 2), 30));
		titleLabel.setFont(sMENUHEADLINEFONT);
		titleLabel.setForeground(sTEXTCOLOR);

		JPanel titlePanel = new JPanel();
		titlePanel.setOpaque(false);
		titlePanel.setLayout(new BorderLayout(0, 0));
		titlePanel.add(titleLabel, BorderLayout.CENTER);
		titlePanel.setMaximumSize(new Dimension((int) (screenDimension.getWidth()), 30));
		titlePanel.setBorder(new EmptyBorder(10, 0, 10, 10));

		JLabel msgLabel = new JLabel("<html>This welcome screen provides quick starting actions, like create a new project, <br> open a recent project, open an example project, and check news and documentation</html>");

		msgLabel.setOpaque(false);
		msgLabel.setMaximumSize(new Dimension((int) (screenDimension.getWidth() / 2), 30));
		msgLabel.setFont(new Font("Helvetica", Font.PLAIN, 18));
		msgLabel.setForeground(sTEXTCOLOR);

		JPanel messagePanel = new JPanel();
		messagePanel.setOpaque(false);
		messagePanel.setLayout(new BorderLayout(0, 0));
		messagePanel.add(msgLabel, BorderLayout.CENTER);
		messagePanel.setBorder(new EmptyBorder(10, 0, 10, 10));

		mLeftProjectBox = Box.createVerticalBox();
		mRightProjectBox = Box.createVerticalBox();

		mCenterProjectBox = Box.createHorizontalBox();

		mLeftProjectBox.setOpaque(false);
		mRightProjectBox.setOpaque(false);
		mCenterProjectBox.setOpaque(false);

		mLeftProjectBox.setMaximumSize(halfScreenDimension);
		mLeftProjectBox.setMinimumSize(halfScreenDimension);
		mLeftProjectBox.setPreferredSize(halfScreenDimension);
		mRightProjectBox.setMaximumSize(halfScreenDimension);
		mRightProjectBox.setMinimumSize(halfScreenDimension);
		mRightProjectBox.setPreferredSize(mLeftProjectBox.getSize());
		mLeftProjectBox.setBorder(new EmptyBorder(0, 0, 0, 1));

		mCenterProjectBox.add(mLeftProjectBox);
		content.add(titlePanel);
		content.add(messagePanel);

		msgLabel.setAlignmentX(LEFT_ALIGNMENT);
		titleLabel.setAlignmentX(LEFT_ALIGNMENT);
		mCenterProjectBox.add(Box.createHorizontalStrut(10));
		mCenterProjectBox.add(mRightProjectBox);

		Dimension centerSize = mCenterProjectBox.getMaximumSize();

		titlePanel.setMaximumSize(new Dimension((int) centerSize.getWidth(), 30));
		messagePanel.setMaximumSize(new Dimension((int) centerSize.getWidth(), 150));

		createRecentAndSamplePrjList();
		content.add(mCenterProjectBox);

		// acquire the build details
		try {
			Properties vProp;
			try (InputStream versionInfo = Preferences.sVSM_VERSIONURL.openStream()) {
				vProp = new Properties();
				vProp.load(versionInfo);
			}
			String vStr = (String) vProp.getProperty("build.version");
			String bStr = (String) vProp.getProperty("build.number");
			String dStr = (String) vProp.getProperty("build.date");

			JLabel versionLabel = new JLabel("Version: " + vStr + "  Build Number: " + bStr + "  Build Date: " + dStr);
			versionLabel.setHorizontalAlignment(SwingConstants.LEFT);
			versionLabel.setOpaque(false);
			versionLabel.setMaximumSize(new Dimension((int) (screenDimension.getWidth() / 2), 30));
			versionLabel.setFont(new Font("Helvetica", Font.PLAIN, 18));
			versionLabel.setForeground(sTEXTCOLOR);
			Dimension d = new Dimension(0, 30);

			content.add(Box.createRigidArea(d));
			content.add(versionLabel);
		} catch (Exception e) {
			e.printStackTrace();
		}

		mParentFrame = null;
		Window windows[] = Window.getWindows();
		for (Window w : windows) {
			if (w instanceof EditorInstance) {
				// There is only one, so ...
				mParentFrame = (EditorInstance) w;
			}
		}

		// put a stickman on the glasspane
		mParentFrame.setGlassPane(new JPanel());
		JPanel p = (JPanel) mParentFrame.getGlassPane();
		p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
		p.setOpaque(false);
		p.add(mWelcomeStickman);

		
                this.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				if (mWelcomeStickman.mHead.getBounds().contains(evt.getX(), evt.getY())) {
					mWelcomeStickman.doAnimation("Speaking", 500, "My head!", false);
					mWelcomeStickman.doAnimation("Blink", 100, false);
					mWelcomeStickman.doAnimation("Mouth_O", 40, true);
					mWelcomeStickman.doAnimation("Mouth_Default", 20, true);
				} else {
					mWelcomeStickman.doAnimation("Blink", 100, false);
					mWelcomeStickman.doAnimation("Smile", 500, false);
				}
			}
		});
                
		// add the content
		add(content);

		setMaximumSize(mEditorInstance.getSize());
		setPreferredSize(mEditorInstance.getSize());
		//setOpaque(true);

		setBackground(new Color(235, 235, 235));
        //setBackground(Color.white);

                
		// Let the user welcome
		new java.util.Timer().schedule(
		  new java.util.TimerTask() {
			  @Override
			  public void run() {
				  mWelcomeStickman.doAnimation("Speaking", 2000, "Welcome!", false);
				  mWelcomeStickman.doAnimation("Mouth_O", 20, true);
				  mWelcomeStickman.doAnimation("Smile", 1000, false);
				  mWelcomeStickman.doAnimation("TiltLeft", 300, false);
				  mWelcomeStickman.doAnimation("WaveLeft", 2000, true);
				  mWelcomeStickman.doAnimation("TiltLeftBack", 300, false);
			  }
		  },
		  1000
		);
                
	}
	
	public final void showWelcomeStickman(boolean show) {
		mShowStickman = show;
		repaint();
	}

	// Draws the image on the background
	@Override
	public final void paintComponent(final Graphics graphics) {
		mWelcomeStickman.setVisible(isVisible() && mShowStickman);
		mParentFrame.getGlassPane().setVisible(isVisible() && mShowStickman);

		super.paintComponent(graphics);

		Graphics2D g2 = (Graphics2D) graphics;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		graphics.drawImage(Preferences.BACKGROUND_IMAGE, -550, -20, null);

		g2.setColor(new Color(0, 0, 0, 64));
		g2.fillRect(0, getBounds().height - 30, getBounds().width, getBounds().height);
	}

	public void updateWelcomePanel() {
		createRecentAndSamplePrjList();
	}

	/**
	 * Creates the list of recent projects
	 */
	public void createRecentAndSamplePrjList() {
		mLeftProjectBox.removeAll();
		mLeftProjectBox.revalidate();
		mLeftProjectBox.repaint();

		mRightProjectBox.removeAll();
		mRightProjectBox.revalidate();
		mRightProjectBox.repaint();
		createMenuButtons();
		listOfRecentProjects();
		listOfTutorials();
		listOfBuildInProjects();
		newsAndDoc();
	}

	/**
	 * Creates open and new project buttons
	 */
	private void createMenuButtons() {
		// PROJECTS SECTION
		JLabel actionMenu = new JLabel(" General");

		actionMenu.setMaximumSize(new Dimension(buttonSize));
		actionMenu.setPreferredSize(new Dimension(buttonSize));
		actionMenu.setOpaque(true);
		actionMenu.setBackground(sMENUHEADLINECOLOR);
		actionMenu.setForeground(sTEXTCOLOR);
		actionMenu.setFont(sMENUHEADLINEFONT);
		mLeftProjectBox.add(actionMenu);

        // *********************************************************************
		// NEW PROJECT BUTTON
		// *********************************************************************
		JLabel mNewProjMenu = new JLabel("New Project");

		mNewProjMenu.setToolTipText("Create New Project");
		mNewProjMenu.setIcon(ResourceLoader.loadImageIcon("/res/img/arrow_icon.png"));
		mNewProjMenu.setMaximumSize(new Dimension(buttonSize));
		mNewProjMenu.setPreferredSize(new Dimension(buttonSize));
		mNewProjMenu.setFont(sMENUITEMFONT);
		mNewProjMenu.setOpaque(true);
		mNewProjMenu.setBackground(sMENUITEMBACKBGROUNDCOLOR);
		mNewProjMenu.setForeground(sTEXTCOLOR);
		mNewProjMenu.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				// mEditorInstance.newProject();
				NewProjectDialog npd = new NewProjectDialog();
				mWelcomeStickman.setVisible(false);
			}

			@Override
			public void mouseEntered(MouseEvent me) {
				me.getComponent().setBackground(sHIGHLIGHTCOLOR);
				EditorStarter.this.repaint();
			}

			@Override
			public void mouseExited(MouseEvent me) {
				me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
				EditorStarter.this.repaint();
			}
		});
		mLeftProjectBox.add(mNewProjMenu);
		mLeftProjectBox.add(new CoolSeparator());

        // *********************************************************************
		// OPEN PROJECT BUTTON
		// *********************************************************************
		JLabel mOpenProjectMenu = new JLabel("Open a Project");

		mOpenProjectMenu.setToolTipText("Open an external Project");
		mOpenProjectMenu.setIcon(ResourceLoader.loadImageIcon("/res/img/arrow_icon.png"));
		mOpenProjectMenu.setMaximumSize(new Dimension(buttonSize));
		mOpenProjectMenu.setPreferredSize(new Dimension(buttonSize));
		mOpenProjectMenu.setOpaque(true);
		mOpenProjectMenu.setBackground(sMENUITEMBACKBGROUNDCOLOR);
		mOpenProjectMenu.setForeground(sTEXTCOLOR);
		mOpenProjectMenu.setFont(sMENUITEMFONT);
		mOpenProjectMenu.addMouseListener(new java.awt.event.MouseAdapter() {
			public void mouseClicked(java.awt.event.MouseEvent evt) {
				mEditorInstance.openProject();
				mWelcomeStickman.setVisible(false);
			}

			public void mouseEntered(MouseEvent me) {
				me.getComponent().setBackground(sHIGHLIGHTCOLOR);
				EditorStarter.this.repaint();
			}

			public void mouseExited(MouseEvent me) {
				me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
				EditorStarter.this.repaint();
			}
		});
		mLeftProjectBox.add(mOpenProjectMenu);
		mLeftProjectBox.add(Box.createVerticalStrut(30));
	}

	/**
	 * creates the box with the list of recent projects
	 */
	private void listOfRecentProjects() {

        // *********************************************************************
		// LIST OF RECENT PROJECTS
		// *********************************************************************
		JLabel titleMenu = new JLabel((Preferences.sMAX_RECENT_FILE_COUNT > 1) ? " Recent Projects" : " Recent Project");

		titleMenu.setBorder(null);
		titleMenu.setFont(sMENUHEADLINEFONT);
		mLeftProjectBox.add(titleMenu);
		titleMenu.setOpaque(true);
		titleMenu.setBackground(sMENUHEADLINECOLOR);
		titleMenu.setForeground(sTEXTCOLOR);
		titleMenu.setMaximumSize(new Dimension(buttonSize));
		titleMenu.setPreferredSize(new Dimension(buttonSize));

		JLabel[] projectList = new JLabel[Preferences.sMAX_RECENT_FILE_COUNT];
		JPanel recentPanel = new JPanel();

		recentPanel.setOpaque(false);
		recentPanel.setLayout(new BoxLayout(recentPanel, BoxLayout.Y_AXIS));

		int filesConsidered = (Preferences.sMAX_RECENT_FILE_COUNT < 5) ? Preferences.sMAX_RECENT_FILE_COUNT : 4;
		for (int i = 0; i <= filesConsidered; i++) {
			String projectDirName = Preferences.getProperty("recentproject." + i + ".path");
			String projectName = Preferences.getProperty("recentproject." + i + ".name");

			if (projectDirName != null) {
				final File projectDir = new File(projectDirName);

				if (projectDir.exists()) {
					if (projectDirName.startsWith("res" + System.getProperty("file.separator") + "prj")) {
						continue;
					}

					String modified = Preferences.getProperty("recentproject." + i + ".date");

					if (modified == null) {
						modified = "Not saved yet";
					}

					projectList[i] = new JLabel(projectName + ", last edited: " + modified);
					projectList[i].setLayout(new BoxLayout(projectList[i], BoxLayout.X_AXIS));
					projectList[i].setOpaque(true);
					projectList[i].setBackground(sMENUITEMBACKBGROUNDCOLOR);
					projectList[i].setForeground(sTEXTCOLOR);
					projectList[i].setMaximumSize(new Dimension(buttonSize));
					projectList[i].setPreferredSize(new Dimension(buttonSize));
					projectList[i].setFont(sMENUITEMFONT);
					projectList[i].setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
                                        projectList[i].setToolTipText(projectDirName);
					projectList[i].addMouseListener(new MouseAdapter() {
						@Override
						public void mouseClicked(MouseEvent me) {
							if(SwingUtilities.isRightMouseButton(me) ){
                                                            RunTimeProject project = new RunTimeProject();
                                                            project.parseForInformation(projectDir.getPath());
                                                            PropertyManagerGUI gui = new PropertyManagerGUI();
                                                            gui.init(project);
                                                            gui.setVisible(true);
                                                        }
                                                        else{
                                                            mEditorInstance.openProject(projectDir.getPath());
                                                            mWelcomeStickman.setVisible(false);
                                                        }
						}

						@Override
						public void mouseEntered(MouseEvent me) {
							me.getComponent().setBackground(sHIGHLIGHTCOLOR);
							EditorStarter.this.repaint();
						}

						@Override
						public void mouseExited(MouseEvent me) {
							me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
							EditorStarter.this.repaint();
						}
					});
					recentPanel.add(projectList[i]);
					recentPanel.add(new CoolSeparator());
				}
			}
		}

		// remove last separator
		if (recentPanel.getComponentCount() > 0) {
			recentPanel.remove(recentPanel.getComponentCount() - 1);
		}

		mLeftProjectBox.add(recentPanel);
	}

	/**
	 * Creates link list of TUTORIAL PROJECTS
	 */
	private void listOfTutorials() {
        // *********************************************************************
		// LIST OF TUTORIALS PROJECTS
		// *********************************************************************

		int tutorialProjCnt = 0;
		if (TutorialsProjFolder.exists()) {
			tutorialProjCnt = TutorialsProjFolder.listFiles().length;
		}
		if (tutorialProjCnt == 0) {
			return;
		}

		JLabel tutorialsMenu = new JLabel((tutorialProjCnt > 1) ? " Tutorials" : " Tutorial");
		tutorialsMenu.setBorder(null);
		tutorialsMenu.setMaximumSize(new Dimension(buttonSize));
		tutorialsMenu.setPreferredSize(new Dimension(buttonSize));
		tutorialsMenu.setOpaque(true);
		tutorialsMenu.setBackground(sMENUHEADLINECOLOR);
		tutorialsMenu.setForeground(sTEXTCOLOR);
		tutorialsMenu.setFont(sMENUHEADLINEFONT);
		mRightProjectBox.add(tutorialsMenu);

		JPanel tutorialProjPanel = new JPanel();

		tutorialProjPanel.setOpaque(false);
		tutorialProjPanel.setLayout(new BoxLayout(tutorialProjPanel, BoxLayout.Y_AXIS));

		JScrollPane mScrollPanel = new JScrollPane(tutorialProjPanel);
		mScrollPanel.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
		mScrollPanel.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		mScrollPanel.setViewportView(tutorialProjPanel);
		mScrollPanel.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
		mScrollPanel.setViewportBorder(new EmptyBorder(0, 0, 0, 0));
		mScrollPanel.setOpaque(false);
		mScrollPanel.getViewport().setOpaque(false);
		mScrollPanel.setAlignmentX(LEFT_ALIGNMENT);

		File listDirs[] = TutorialsProjFolder.listFiles();

		for (final File tutorialDir : listDirs) {

			final File tutorialProj = new File(tutorialDir.getPath());

			if (tutorialProj.exists()) {
				//File projectPath = new File(sampleDir.getPath() + "project.xml" );
				RunTimeProject project = new RunTimeProject();

				project.parseForInformation(tutorialProj.getPath());
				JLabel newTutorialProj = new JLabel(project.getProjectName() + " [" + 
                                        project.getSceneFlow().getNodeAndSuperNodeList().size() + 
                                        " global nodes, " +
                                        project.getSceneScript().copySceneList().size() +
                                        " scenes, " +
                                        project.getProjectConfig().getPluginConfigList().size() + 
                                        " plugins, " +
                                        project.getProjectConfig().getAgentNames().size() + " agents]");// + ", last edited: "
				// + Preferences.sDATE_FORMAT.format(tutorialProj.lastModified()));

				newTutorialProj.setLayout(new BoxLayout(newTutorialProj, BoxLayout.X_AXIS));
				newTutorialProj.setMaximumSize(new Dimension(buttonSize));
				newTutorialProj.setPreferredSize(new Dimension(buttonSize));
				newTutorialProj.setFont(sMENUITEMFONT);
				newTutorialProj.setOpaque(true);
				newTutorialProj.setBackground(sMENUITEMBACKBGROUNDCOLOR);
				newTutorialProj.setForeground(sTEXTCOLOR);
				newTutorialProj.setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
				newTutorialProj.addMouseListener(new MouseAdapter() {
					@Override
					public void mouseClicked(MouseEvent me) {
                                                
						if(SwingUtilities.isRightMouseButton(me) ){
                                                    PropertyManagerGUI gui = new PropertyManagerGUI();
                                                    gui.init(project);
                                                    gui.setVisible(true);
                                                }else{
                                                    mEditorInstance.openProject(tutorialProj.getPath());
                                                    mWelcomeStickman.setVisible(false);
                                                }
						
					}

					@Override
					public void mouseEntered(MouseEvent me) {
						me.getComponent().setBackground(sHIGHLIGHTCOLOR);
						EditorStarter.this.repaint();
					}

					@Override
					public void mouseExited(MouseEvent me) {
						me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
						EditorStarter.this.repaint();
					}
				});
				tutorialProjPanel.add(newTutorialProj);

				tutorialProjPanel.add(new CoolSeparator());
			}
		}

		// remove last separator
		if (tutorialProjPanel.getComponentCount() > 0) {
			tutorialProjPanel.remove(tutorialProjPanel.getComponentCount() - 1);
		}

		mRightProjectBox.add(mScrollPanel);
		mScrollPanel.setMaximumSize(tutorialProjPanel.getMaximumSize());

	}

	/**
	 * Creates link list of buildin sample projects
	 */
	private void listOfBuildInProjects() {
        // *********************************************************************
		// LIST OF SAMPLE PROJECTS
		// *********************************************************************

		CodeSource src = SceneMaker3.class.getProtectionDomain().getCodeSource();
		List<String> listDirs = new ArrayList<String>();
		String path = "res/prj";
		if (src != null) {
			URL jar = src.getLocation();
			ZipInputStream zip = null;
			try {
				zip = new ZipInputStream(jar.openStream());
			} catch (IOException ex) {
				mLogger.failure("Error reading resource");

			}
			ZipEntry ze = null;

			try {
				while ((ze = zip.getNextEntry()) != null) {
					String entryName = ze.getName();
					if (entryName.startsWith(path) && ze.isDirectory()) {
						String entry = entryName.substring(path.length() + 1);
						int checkSubdir = entry.indexOf("/");
						if (checkSubdir >= 0) {
							// if it is a subdirectory, we just return the directory name
							entry = entry.substring(0, checkSubdir);
						}
						if (!listDirs.contains(entry) && !entry.equals("")) {
							listDirs.add(path + "/" + entry);
//                            System.out.println(entry);
						}
					}
				}
			} catch (IOException ex) {
				mLogger.failure("Error reading resource");
			}

		}
		int buildInSampleProjCnt = listDirs.size();

		if (buildInSampleProjCnt == 0) {
			return;
		}

		JLabel exampleMenu = new JLabel((buildInSampleProjCnt > 1) ? " Built-in Sample Projects" : " Built-in Sample Project");
		exampleMenu.setBorder(null);
		exampleMenu.setMaximumSize(new Dimension(buttonSize));
		exampleMenu.setPreferredSize(new Dimension(buttonSize));
		exampleMenu.setOpaque(true);
		exampleMenu.setBackground(sMENUHEADLINECOLOR);
		exampleMenu.setForeground(sTEXTCOLOR);
		exampleMenu.setFont(sMENUHEADLINEFONT);
		mRightProjectBox.add(exampleMenu);

		JPanel sampleProjPanel = new JPanel();
		sampleProjPanel.setOpaque(false);
		sampleProjPanel.setLayout(new BoxLayout(sampleProjPanel, BoxLayout.Y_AXIS));

		JScrollPane mScrollPanel = new JScrollPane(sampleProjPanel);
		mScrollPanel.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
		mScrollPanel.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		mScrollPanel.setViewportView(sampleProjPanel);
		mScrollPanel.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
		mScrollPanel.setViewportBorder(new EmptyBorder(0, 0, 0, 0));
		mScrollPanel.setOpaque(false);
		mScrollPanel.getViewport().setOpaque(false);
		mScrollPanel.setAlignmentX(LEFT_ALIGNMENT);

		for (final String sampleDir : listDirs) {
                        RunTimeProject project = new RunTimeProject();

			project.parseForInformation(sampleDir);

			JLabel newSampleProj = new JLabel(project.getProjectName() + "                         ");

			newSampleProj.setLayout(new BoxLayout(newSampleProj, BoxLayout.X_AXIS));
			newSampleProj.setMaximumSize(new Dimension(buttonSize));
			newSampleProj.setPreferredSize(new Dimension(buttonSize));
			newSampleProj.setFont(sMENUITEMFONT);
			newSampleProj.setOpaque(true);
			newSampleProj.setBackground(sMENUITEMBACKBGROUNDCOLOR);
			newSampleProj.setForeground(sTEXTCOLOR);
			newSampleProj.setIcon(ResourceLoader.loadImageIcon("/res/img/dociconsmall.png"));
			newSampleProj.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseClicked(MouseEvent me) {
					// mEditorInstance.toggleProjectEditorList(true);
					mEditorInstance.openProject(sampleDir);
					mWelcomeStickman.setVisible(false);
				}

				@Override
				public void mouseEntered(MouseEvent me) {
					me.getComponent().setBackground(sHIGHLIGHTCOLOR);
					EditorStarter.this.repaint();
				}

				@Override
				public void mouseExited(MouseEvent me) {
					me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
					EditorStarter.this.repaint();
				}
			});
			sampleProjPanel.add(newSampleProj);

			sampleProjPanel.add(new CoolSeparator());

		}

		// remove last separator
		if (sampleProjPanel.getComponentCount() > 0) {
			sampleProjPanel.remove(sampleProjPanel.getComponentCount() - 1);
		}

		mRightProjectBox.add(mScrollPanel);
		mScrollPanel.setMaximumSize(sampleProjPanel.getMaximumSize());
	}

	/**
	 * Adds information items
	 */
	private void newsAndDoc() {

        // *********************************************************************
		// NEWS AND DOCUMENTATION
		// *********************************************************************
		JLabel mDocuMenu = new JLabel(" News and Documentation");

		mDocuMenu.setToolTipText("News and Documentation online");
		mDocuMenu.setMaximumSize(new Dimension(buttonSize));
		mDocuMenu.setPreferredSize(new Dimension(buttonSize));
		mDocuMenu.setOpaque(true);
		mDocuMenu.setBackground(sMENUHEADLINECOLOR);
		mDocuMenu.setForeground(sTEXTCOLOR);
		mDocuMenu.setFont(sMENUHEADLINEFONT);
		mRightProjectBox.add(mDocuMenu);

		JLabelURL link = new JLabelURL("Visual SceneMaker Online", "http://scenemaker.dfki.de/");

		link.setToolTipText("Go to the VisualSceneMaker web page");
		link.setIcon(ResourceLoader.loadImageIcon("/res/img/arrow_icon.png"));
		link.setMaximumSize(new Dimension(buttonSize));
		link.setPreferredSize(new Dimension(buttonSize));
		link.setOpaque(true);
		link.setBackground(sMENUITEMBACKBGROUNDCOLOR);
		link.setForeground(sTEXTCOLOR);
		link.setFont(sMENUITEMFONT);
		link.addMouseListener(new java.awt.event.MouseAdapter() {
			@Override
			public void mouseEntered(MouseEvent me) {
				me.getComponent().setBackground(sHIGHLIGHTCOLOR);
				EditorStarter.this.repaint();
			}

			@Override
			public void mouseExited(MouseEvent me) {
				me.getComponent().setBackground(sMENUITEMBACKBGROUNDCOLOR);
				EditorStarter.this.repaint();
			}
		});

		mRightProjectBox.add(link);
	}

	public void refresh() {
		// noting to do ...
	}
}
