package de.dfki.vsm.players.server;

import de.dfki.vsm.players.ActionPlayer;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class TCPActionServer extends Thread {

	public static int mServerPort = 7777;
	public static ServerSocket mServerSocket = null;

	private static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
	private static TCPActionServer sInstance = null;
	private static List<ServerConnectionHandler> sClientConnections = Collections.synchronizedList(new ArrayList());
	private final ArrayList<AnimationListener> mAnimationListeners = new ArrayList<>();
	private boolean mRunning = true;

	private TCPActionServer() {
		setName("VSM SceneMaker Action Server");
	}

	public static TCPActionServer getInstance() {
		if (sInstance == null) {
			sInstance = new TCPActionServer();
		}
		return sInstance;
	}

	public void addListener(AnimationListener al) {
		mAnimationListeners.add(al);
	}

	public void removeListener(AnimationListener al) {
		synchronized (mAnimationListeners) {
			if (mAnimationListeners.contains(al)) {
				mAnimationListeners.remove(al);
			}
		}
	}

	public void notifyListeners(String animID) {
		synchronized (mAnimationListeners) {
			mAnimationListeners.stream().forEach((al) -> {
				//mLogger.info("Listener information about Animation " + a.toString() + " with id " + a.mID);
				al.update(animID);
			});
		}
	}

	public void end() {
		sClientConnections.stream().forEach((c) -> {
			c.end();
		});

		mRunning = false;
		try {
			mServerSocket.close();
		} catch (IOException ex) {
			mLogger.warning(ex.getMessage());
		}

		ActionPlayer.mActionServerRunning = false;

		sInstance = null;
	}

	public void send(String message) {
		sClientConnections.stream().forEach((c) -> {
			c.sendToApplication(message);
		});
	}

	@Override
	public void run() {
		try {
			mServerSocket = new ServerSocket(mServerPort);
			mLogger.message(getName() + " starts listening ...");
			ActionPlayer.mActionServerRunning = true;

			while (mRunning) {
				Socket clientSocket = mServerSocket.accept();
				ServerConnectionHandler c = new ServerConnectionHandler(clientSocket);
				c.start();
				sClientConnections.add(c);
			}
		} catch (IOException ex) {
			mLogger.warning(ex.getMessage());
		}
	}
}
