package de.dfki.vsm.players.server;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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

	public void end() {
		sClientConnections.stream().forEach((c) -> {
			c.end();
		});

		mRunning = false;
		try {
			mServerSocket.close();
		} catch (IOException ex) {
			mLogger.message(ex.getMessage());
		}

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
			//mLogger.message(getName() + " starts listening ..." + mRunning);

			while (mRunning) {
				Socket clientSocket = mServerSocket.accept();
				ServerConnectionHandler c = new ServerConnectionHandler(clientSocket);
				c.start();
				sClientConnections.add(c);
			}
		} catch (IOException ex) {
			mLogger.message(ex.getMessage());
		}
	}
}
