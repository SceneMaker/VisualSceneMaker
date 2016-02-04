package de.dfki.vsm.players.server;

import de.dfki.vsm.players.ActionPlayer;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import de.dfki.vsm.players.stickman.animationlogic.listener.AnimationListener;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class TCPActionServer extends Thread {

	public static int mServerPort = 7777;
	public static ServerSocket mServerSocket = null;

	private static final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
	private static TCPActionServer sInstance = null;
        private List<ServerConnectionHandler> sClientConnections =  new CopyOnWriteArrayList<ServerConnectionHandler>();
	//private static List<ServerConnectionHandler> sClientConnections = Collections.synchronizedList(new ArrayList());
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
				al.update(animID);
			});
		}
	}

	public void end() {
		sClientConnections.stream().forEach((c) -> {
			c.end();
		});
                
                // be sure all connection are done ...
                sClientConnections = new CopyOnWriteArrayList<ServerConnectionHandler>();
                
		mRunning = false;
		try {
			mServerSocket.close();
		} catch (IOException ex) {
			mLogger.warning(ex.getMessage());
		}

		ActionPlayer.mActionServerRunning = false;

		sInstance = null;
	}

        public Set<String> getConnectionIDs() {
            Set<String> result = new HashSet<>();
            sClientConnections.stream().forEach((s) -> {
                result.add(s.mClientId);
            });
            return result;
        }
        
	public void sendToAll(String message) {
		sClientConnections.stream().forEach((c) -> {
			c.sendToApplication(message);
		});
	}

	public void sendToClient(String id, String message) {
		sClientConnections.stream().forEach((c) -> {
			if (c.mClientId.equalsIgnoreCase(id)) {
				c.sendToApplication(message);
			}
		});
	}

	public boolean hasClient(String id) {
		for (ServerConnectionHandler c : sClientConnections) {
			if (c.mClientId.equalsIgnoreCase(id)) {
				return true;
			}
		}
		return false;
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
