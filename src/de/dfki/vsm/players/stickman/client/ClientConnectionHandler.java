package de.dfki.vsm.players.stickman.client;

import de.dfki.vsm.players.stickman.StickmanStage;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketAddress;
import java.net.UnknownHostException;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ClientConnectionHandler extends Thread {

	private Socket mSocket;
	private String mHost = "127.0.0.1";
	private int mPort = 7777;
	private PrintWriter mOut;
	private BufferedReader mIn;

	private boolean mRunning = true;

	public ClientConnectionHandler() {
		super.setName("StickmanStage Socket Connection Handler");
	}

	public void end() {
		try {
			mSocket.shutdownInput();
			mSocket.shutdownOutput();
			mSocket.close();
			mRunning = false;
		} catch (IOException ex) {
			StickmanStage.mLogger.severe("Error closing socket to " + mHost + ", " + mPort);
		}
	}

	public void sendToServer(String message) {
		if (mSocket.isConnected()) {
			mOut.println(message);
			mOut.flush();
		}
	}

	public void connect() {
		try {
			InetAddress inteAddress = InetAddress.getByName(mHost);
			SocketAddress socketAddress = new InetSocketAddress(inteAddress, mPort);

			mSocket = new Socket();
			mSocket.connect(socketAddress, 2000); // wait max. 2000ms

			mOut = new PrintWriter(mSocket.getOutputStream(), true);
			mIn = new BufferedReader(new InputStreamReader(mSocket.getInputStream()));
		} catch (UnknownHostException e) {
			StickmanStage.mLogger.severe(mHost + " is unknown - aborting!");
		} catch (IOException e) {
			StickmanStage.mLogger.severe(mHost + " i/o exception - aborting!");
		}
	}

	public void run() {
		String input = "";
		
		while (mRunning) {
			try {
				input = mIn.readLine();

				if (input != null) {
					input = input.trim();
					if (!input.isEmpty()) {
						StickmanStage.getInstance().parseStickmanMLCmd(input);
					}
				}
			} catch (IOException ex) {
				StickmanStage.mLogger.severe(mHost + " i/o exception - aborting!");
			}
		}
	}
}