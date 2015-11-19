package de.dfki.vsm.players.server;

import de.dfki.vsm.players.EventActionPlayer;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ServerConnectionHandler extends Thread {

	private Socket mClientSocket;
	private PrintWriter mOut;
	private BufferedReader mIn;
	private boolean mRunning = true;

	private static final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

	public ServerConnectionHandler(Socket s) {
		super.setName("Server Connection Handler");
		try {
			mClientSocket = s;

			mOut = new PrintWriter(mClientSocket.getOutputStream(), true);
			mIn = new BufferedReader(new InputStreamReader(mClientSocket.getInputStream()));
		} catch (IOException e) {
			mLogger.message("Client connection could not properly established " + e.getMessage());
		}
	}

	public void sendToApplication(String message) {
		if (mClientSocket.isConnected()) {
			mOut.println(message);
			mOut.flush();
		}
	}

	public void end() {
		//mLogger.message("Shutting down client connection ...");
		try {
			mClientSocket.shutdownInput();
			mClientSocket.shutdownOutput();
			mClientSocket.close();
			mRunning = false;
		} catch (IOException ex) {
			mLogger.failure("VSM ActionServer i/o exception during closing client connection");
		}
	}

	@Override
	public void run() {
		mLogger.message("VSM ActionServer listening to " + mClientSocket.toString());

		String input = "";

		while (mRunning) {
			try {
				input = mIn.readLine();

				if (input != null) {
					input = input.trim();
					if (!input.isEmpty()) {
						mLogger.message("Receiving " + input);

						if (input.contains("#TM")) {
							EventActionPlayer.getInstance().runActionAtTimeMark(input);
						}
						if (input.contains("#ANIM")) {
							int start = input.lastIndexOf("#") + 1;
							String id = input.substring(start);
							TCPActionServer.getInstance().notifyListeners(id);
						}
					}
				}
			} catch (IOException ex) {
				mLogger.failure("VSM ActionServer connection i/o exception!");
			}
		}
	}
}
