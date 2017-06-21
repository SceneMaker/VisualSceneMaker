package de.dfki.vsm.xtension.baxter.utils.communication;

import de.dfki.vsm.xtension.baxter.BaxterHandler;

import java.io.*;
import java.net.*;
import java.util.LinkedList;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

/**
 * Created by alvaro on 6/1/16.
 */
public class BaxterServerProcess extends Observable {
    private final String processName = "baxter_server";
    private final String serverBasePath;
    private LinkedList<Observer> observers = new LinkedList<>();
    public BaxterServerProcess(String basePath){
        serverBasePath = basePath;
    }
    public int  unloadBaxterServer() throws IOException, InterruptedException {
        Process killer = null;
        final String killCmd = "ps aux | grep '" + processName + "' | awk '{print $2}' | xargs kill";
        String[] cmd = {"/bin/sh", "-c", killCmd};
        killer = Runtime.getRuntime().exec(cmd);
        int value = killer.waitFor();
        return value;
    }

    private String[] getServerCmdPath() throws FileNotFoundException {
        if(serverBasePath == null || serverBasePath.equals("")){
            throw new FileNotFoundException("Baxter Server not found");
        }
        File f = new File(serverBasePath);
        if(!f.exists()) {
            throw new FileNotFoundException("Baxter Server not found");
        }
        String cmdPath[] = {"python", serverBasePath, "> /dev/null 2>&1"};
        return cmdPath;
    }

    public void launchBaxterServer() throws Exception {
        String processName = "imageviwer";
        String []serverCmdPath = getServerCmdPath();

        final ProcessBuilder processB = new ProcessBuilder(serverCmdPath);
        processB.redirectErrorStream(true);
        Process p;
        try {
            p = processB.start();
            boolean started = false;
            while (!started) {
                try {
                    ServerSocket serverSocket = new ServerSocket(1313);
                    serverSocket.close();
                    Thread.sleep(100);
                } catch (IOException e) {
                   started = true;
                    notifyAllObservers("Server started");
                }

            }
        } catch (IOException e) {
            e.printStackTrace();
            notifyAllObservers("Server could not be started. Reason: " + e.getMessage());
            throw new Exception("Baxter SErver could not be started");
        }
    }

    public Socket connectToSocket() throws IOException {
        InetAddress inteAddress = null;
        inteAddress = InetAddress.getByName("localhost");
        SocketAddress socketAddress = new InetSocketAddress(inteAddress, 1313);
        Socket mSocket = new Socket();
        mSocket.connect(socketAddress, 2000); // wait max. 2000ms

        return mSocket;
    }

    public void registerObserver(Observer observer){
        observers.add(observer);
    }

    public void notifyAllObservers(String message){
        for (Observer o: observers  ) {
            o.update(this, message);
        }
    }


}
