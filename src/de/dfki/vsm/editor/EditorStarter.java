package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.NewProjectDialog;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSeparator;

/**
 * 
 * @author mfallas, Patrick Gebhard
 *
 * Class implements welcome screen with link list of recent projects and sample projects
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

	private final File SampleProjFolder = new File(Preferences.sSAMPLE_PROJECTS);

	private final EditorInstance mEditorInstance;
	private final Box mRecentProjects;

	// The singelton logger instance   
	private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

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
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		setBorder(BorderFactory.createEmptyBorder(paddingSize, paddingSize, paddingSize, paddingSize));

		JLabel titleLabel = new JLabel("Welcome to Visual SceneMaker");

		titleLabel.setOpaque(false);
		titleLabel.setFont(sMENUHEADLINEFONT);
		titleLabel.setForeground(sTEXTCOLOR);

		JLabel msgLabel = new JLabel("<html>This welcome screen provides quick starting actions, like a new project, <br> open a recent project, open a example project, and check news and documentation</html>");

		msgLabel.setOpaque(false);
		msgLabel.setMaximumSize(new Dimension((int) (screenDimension.getWidth() / 2), 30));
		msgLabel.setFont(new Font("Helvetica", Font.PLAIN, 18));
		msgLabel.setForeground(sTEXTCOLOR);
		msgLabel.setBorder(BorderFactory.createEmptyBorder(10, 0, 30, 0));
		mRecentProjects = Box.createVerticalBox();
		mRecentProjects.setOpaque(false);

		mRecentProjects.setMaximumSize(halfScreenDimension);
		createListOfRecentProj();
		add(titleLabel);
		add(msgLabel);

		add(mRecentProjects);
		setMaximumSize(mEditorInstance.getSize());
		setPreferredSize(mEditorInstance.getSize());
		//setOpaque(true);

		setBackground(new Color(235, 235, 235));
		//setBackground(Color.white);
	}

	// Draws the image on the background 
	@Override
	public final void paintComponent(final Graphics graphics) {
		super.paintComponent(graphics);

		Graphics2D g2 = (Graphics2D) graphics;
		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		graphics.drawImage(Preferences.BACKGROUND_IMAGE, -550, -20, null);
	}

	public void updateWelcomePanel() {
		createListOfRecentProj();
	}

	/**
	 * Creates the list of recent projects
	 */
	public void createListOfRecentProj() {
		mRecentProjects.removeAll();
		mRecentProjects.revalidate();
		mRecentProjects.repaint();
		createMenuButtons();
		listOfRecentProjects();
		listOfSampleProjects();
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
		mRecentProjects.add(actionMenu);

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
				new NewProjectDialog();
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
		mRecentProjects.add(mNewProjMenu);
		mRecentProjects.add(new CoolSeparator());

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
		mRecentProjects.add(mOpenProjectMenu);
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
		mRecentProjects.add(titleMenu);
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
					projectList[i].addMouseListener(new MouseAdapter() {
						@Override
						public void mouseClicked(MouseEvent me) {
							// mEditorInstance.toggleProjectEditorList(true);
							mEditorInstance.openProject(projectDir);
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

		mRecentProjects.add(recentPanel);
	}

	/**
	 * Creates link list of sample projects
	 */
	private void listOfSampleProjects() {
		// *********************************************************************
		// LIST OF SAMPLE PROJECTS
		// *********************************************************************

		int sampleProjCnt = 0;
		if (SampleProjFolder.exists()) {
			sampleProjCnt = SampleProjFolder.listFiles().length;
		}
		System.out.println(sampleProjCnt);
		JLabel exampleMenu = new JLabel((sampleProjCnt > 1) ? " Sample Projects" : " Sample Project");

		exampleMenu.setBorder(null);
		exampleMenu.setMaximumSize(new Dimension(buttonSize));
		exampleMenu.setPreferredSize(new Dimension(buttonSize));
		exampleMenu.setOpaque(true);
		exampleMenu.setBackground(sMENUHEADLINECOLOR);
		exampleMenu.setForeground(sTEXTCOLOR);
		exampleMenu.setFont(sMENUHEADLINEFONT);
		mRecentProjects.add(exampleMenu);

		JPanel sampleProjPanel = new JPanel();

		sampleProjPanel.setOpaque(false);
		sampleProjPanel.setLayout(new BoxLayout(sampleProjPanel, BoxLayout.Y_AXIS));

		if (SampleProjFolder.exists()) {
			File listDirs[] = SampleProjFolder.listFiles();

			for (final File sampleDir : listDirs) {

				final File sampleProj = new File(sampleDir.getPath() + "/vsm");

				if (sampleProj.exists()) {
					File projectPath = new File(sampleDir.getPath() + "/vsm/" /* + "config.xml" */);
					EditorProject project = new EditorProject();

					project.parse(projectPath);

					JLabel newSampleProj = new JLabel(project.getProjectName() + ", last edited: "
					  + Preferences.sDATE_FORMAT.format(sampleProj.lastModified()));

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
							mEditorInstance.openProject(sampleProj);
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
			}
		}

		// remove last separator
		if (sampleProjPanel.getComponentCount() > 0) {
			sampleProjPanel.remove(sampleProjPanel.getComponentCount() - 1);
		}

		mRecentProjects.add(sampleProjPanel);
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
		mRecentProjects.add(mDocuMenu);

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

		mRecentProjects.add(link);
	}

	public void refresh() {
		// noting to do ...
	}
}
