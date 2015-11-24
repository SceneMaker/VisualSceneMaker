package de.dfki.vsm.players.stickman;

import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationLoader;
import de.dfki.vsm.players.stickman.animationlogic.EventAnimation;
import de.dfki.vsm.players.stickman.client.ClientConnectionHandler;
import de.dfki.vsm.players.stickman.util.Names;
import de.dfki.vsm.players.stickman.util.StickmanStageLayout;
import de.dfki.vsm.util.xml.XMLUtilities;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.io.ByteArrayInputStream;
import java.nio.charset.Charset;
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
	public static boolean mUseNetwork = false;
	private static String sHost = "127.0.0.1";
	private static int sPort = 7777;
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

		if (mUseNetwork) {
			mConnection = new ClientConnectionHandler();
			mConnection.connect(sHost, sPort);

			while (!mConnection.mConnected) {
				try {
					mLogger.info("Waiting for connection to control application ...");
					Thread.sleep(250);
				} catch (InterruptedException ex) {
					mLogger.severe(ex.getMessage());
				}
			}
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
		mUseNetwork = true;
		return getInstance();
	}

	public static StickmanStage getNetworkInstance(String host, int port) {
		sHost = host;
		sPort = port;
		
		mUseNetwork = true;
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

		if (mUseNetwork) {
			mConnection.end();
		}

		sInstance = null;
	}

	public static void animate(String stickmanname, String type, String name, int duration, String text, boolean block) {
		Stickman sm = getStickman(stickmanname);
		sm.doAnimation(name, duration, text, block);
	}

	public static void parseStickmanMLCmd(String cmd) {
		// TODO cut the crap with the two animation types ...
		Animation a = (cmd.contains("StickmanEventAnimation")) ? new EventAnimation() : new Animation();

		boolean r = XMLUtilities.parseFromXMLStream(a, new ByteArrayInputStream(cmd.getBytes(Charset.forName("UTF-8"))));

		String stickmanname = a.mStickmanName;
		String animationname = a.mName;
		String id = a.mID;
		int duration = a.mDuration;
		boolean blocking = a.mBlocking;
		Object parameter = a.mParameter;

		a = (a instanceof EventAnimation)
		  ? AnimationLoader.getInstance().loadEventAnimation(getStickman(stickmanname), animationname, duration, blocking)
		  : AnimationLoader.getInstance().loadAnimation(getStickman(stickmanname), animationname, duration, blocking);

		a.setID(id); // give the animation the same id (TODO - This is bad design and caused that the animation has to be "reloaded"
		a.mParameter = parameter;

		a.mStickman.playAnimation(a);
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
