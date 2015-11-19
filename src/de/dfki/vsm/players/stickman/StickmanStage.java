package de.dfki.vsm.players.stickman;

import de.dfki.vsm.players.stickman.client.ClientConnectionHandler;
import de.dfki.vsm.players.stickman.util.Names;
import de.dfki.vsm.players.stickman.util.StickmanStageLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class StickmanStage extends JFrame implements MouseListener {

	static private final HashMap<String, Stickman> sStickmansOnStage = new HashMap<>();
	static private JPanel sStickmanPanel;
	static private StickmanStage sInstance;
	private static double sScale = 1.0d;
	// network interface
	public static ClientConnectionHandler mConnection;
	public static boolean mUsingNetwork = false;
	// logging
	public static final Logger mLogger = Logger.getAnonymousLogger();

	private StickmanStage() {
		super("Stickman Stage");
		setResizable(false);

		sStickmanPanel = new JPanel() {
			@Override
			protected void paintComponent(Graphics g) {
				super.paintComponent(g);

				Graphics2D g2 = (Graphics2D) g;
				g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

				AffineTransform at = g2.getTransform();
				at.scale(sScale, sScale);
				g2.setTransform(at);
			}
		};

		sStickmanPanel.setLayout(new StickmanStageLayout(sScale));
		add(sStickmanPanel);

		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);

		ConsoleHandler ch = new ConsoleHandler();
		ch.setFormatter(new StickmanStageLogFormatter());

		if (mUsingNetwork) {
			mConnection = new ClientConnectionHandler();
			mConnection.connect();
		}

		addMouseListener(this);
	}

	public static StickmanStage getInstance() {

		if (sInstance == null) {
			sInstance = new StickmanStage();
		}
		return sInstance;
	}

	public static StickmanStage getNetworkInstance() {
		mUsingNetwork = true;
		return getInstance();
	}

	public static void addStickman(String name) {
		Stickman.TYPE gender = null;
		if (Names.sFemaleNames.contains(name.toLowerCase())) {
			gender = Stickman.TYPE.FEMALE;
		}

		if (Names.sMaleNames.contains(name.toLowerCase())) {
			gender = (gender == null) ? Stickman.TYPE.MALE : gender;
		}

		addStickman(name, gender);
	}

	public static void addStickman(String name, Stickman.TYPE gender) {
		if (!sStickmansOnStage.containsKey(name.toLowerCase())) {
			sStickmansOnStage.put(name.toLowerCase(), new Stickman(name, gender));
			sStickmanPanel.add(getStickman(name));
			sStickmanPanel.revalidate();
		}

		// resize the stuff ...
		StickmanStage.getInstance().pack();
		StickmanStage.getInstance().setVisible(true);
	}

	public static Stickman getStickman(String name) {
		Stickman sm;
		if (sStickmansOnStage.containsKey(name.toLowerCase())) {
			return sStickmansOnStage.get(name.toLowerCase());
		} else {
			return null;
		}
	}

	public static void clearStage() {
		Set<String> deleteStickman = new HashSet<>();
		sStickmansOnStage.keySet().stream().map((s) -> {
			deleteStickman.add(s);
			return s;
		}).forEach((s) -> {
			getStickman(s).mAnimationScheduler.end();
		});
		deleteStickman.stream().map((s) -> {
			sStickmanPanel.remove(getStickman(s));
			return s;
		}).forEach((s) -> {
			sStickmansOnStage.remove(s);
		});

		// resize the stuff ...
		StickmanStage.getInstance().pack();
		StickmanStage.getInstance().setVisible(false);

		if (mUsingNetwork) {
			mConnection.end();
		}

		sInstance = null;
	}

	public static void animate(String stickmanname, String type, String name, int duration, String text, boolean block) {
		Stickman sm = getStickman(stickmanname);
		sm.doAnimation(name, duration, text, block);
	}

	public static void parseStickmanMLCmd(String cmd) {
		mLogger.info("StickmanStage got " + cmd + " as input");
	}

	public static void sendTimeMarkInformation(String timemark) {
		if (mConnection.mConnected) {
			mConnection.sendToServer(timemark);
		}
	}

	public static void sendAnimationUpdate(String state, String id) {
		if (mConnection.mConnected) {
			mConnection.sendToServer("#ANIM#" + state + "#" + id);
		}
	}

	/**
	 * @param args the command line arguments
	 */
	public static void main(String[] args) {
		StickmanStage.getInstance();
		StickmanStage.addStickman("Anna");
		StickmanStage.addStickman("Bob");
	}

	@Override
	public void mouseClicked(MouseEvent e) {
		//getStickman("Anna").mLogger.info("mouse clicked");
		//getStickman("Anna").doAnimation("environment", "Speaking", 3000, "Stell Dir vor, Du kommst nach Hause, und ein Pferd steht in der Küche.", false);
//////		//smM.doAnimation("gesture", "waveleft", false);
		//getStickman("Anna").doAnimation("gesture", "waveleft", 70, false);
//		getStickman("Anna").doAnimation("head", "lookright", 300, true);
//		getStickman("Anna").doAnimation("gesture", "CoverMouth", true);
//		getStickman("Anna").doAnimation("head", "lookleft", 300, true);
		//getStickman("Anna").doAnimation("gesture", "WaveLeft", 50, true);
//		getStickman("Anna").doAnimation("head", "Blink", true);
////		//smM.doAnimation("environment", "speak", "Stell Dir vor, Du kommst nach Hause, und ein Pferd steht in der Küche.", false);
//		getStickman("Anna").doAnimation("environment", "speak", 300, "Stell Dir vor, Du kommst nach Hause, und ein Pferd steht in der Küche.", false);
//////		//smM.doAnimation("face", "Mouth_O", true);
//////		smF.doAnimation("face", "Mouth_O", true);
//////		//smM.doAnimation("head", "TiltLeft", true);
//////		smF.doAnimation("head", "TiltLeft", true);
//////		//smM.doAnimation("head", "Blink", false);
//////		smF.doAnimation("head", "Blink", false);
//////		//smM.doAnimation("face", "Mouth_Default", false);
//////		smF.doAnimation("face", "Mouth_Default", false);
//////		//smM.doAnimation("head", "TiltLeftBack", true);
//////		smF.doAnimation("head", "TiltLeftBack", true);
//
//		getStickman("Anna").doAnimation("face", "Smile", 2000, false);
//		getStickman("Anna").doAnimation("head", "TiltLeft", true);
//		getStickman("Anna").doAnimation("head", "Blink", false);
//		getStickman("Anna").doAnimation("gesture", "WaveLeft", 2000, false);
//
//		getStickman("Anna").doAnimation("head", "TiltLeftBack", true);
//		getStickman("Anna").doAnimation("head", "TiltLeft", true);
//		getStickman("Anna").doAnimation("head", "TiltLeftBack", true);
//		//smF.doAnimation("gesture", "CoverMouth", true);
//////		smF.doAnimation("gesture", "waveleft", false);
//////		smF.doAnimation("head", "TiltLeft", true);
//////		smF.doAnimation("head", "TiltLeftBack", false);
////		smF.doAnimation("gesture", "waveleft", false);
////		smF.doAnimation("head", "TiltLeft", true);
//		getStickman("Anna").doAnimation("head", "Blink", true);
//
//		smF.doAnimation("face", "smile", true);
//		smF.doAnimation("head", "TiltLeftBack", true);
		//smF.doAnimation("face", "smile", true);
//		smF.doAnimation("head", "Blink", false);
//		smF.doAnimation("head", "Nod", true);
//
//		smF.doAnimation("head", "TiltLeft", true);
//
//		smF.doAnimation("head", "Blink", false);
////
//		smF.doAnimation("head", "TiltLeftBack", true);
//		smF.doAnimation("head", "nod", true);
	}

	@Override
	public void mousePressed(MouseEvent e) {

	}

	@Override
	public void mouseReleased(MouseEvent e) {

	}

	@Override
	public void mouseEntered(MouseEvent e) {

	}

	@Override
	public void mouseExited(MouseEvent e) {

	}

	private static class StickmanStageLogFormatter extends Formatter {

		@Override
		public String format(LogRecord record) {
			return ((new StringBuffer()).append(record.getLevel()).append(": ").append(record.getMessage()).append("\n")).toString();
		}
	}
}
