package de.dfki.vsm.util.tts.marytts;

import java.io.*;
import java.net.ServerSocket;
import java.util.LinkedList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

/**
 * Created by alvaro on 5/24/16.
 */
public class MaryTTsProcess extends Observable{
    private String maryBase;
    private final String OS = System.getProperty("os.name").toLowerCase();
    final String instanceExecutedName = "marytts.server.Mary";
    private LinkedList<Observer> observers = new LinkedList<>();
    private static MaryTTsProcess sInstance = null;
    private static  boolean serverStarted = false;
    private MaryTTsProcess(String pMaryBase){
        maryBase = pMaryBase;
    }

    public static MaryTTsProcess getsInstance(String pMaryBase){
        if(sInstance == null){
            sInstance = new MaryTTsProcess(pMaryBase);
        }
        return sInstance;
    }

    public synchronized boolean startMaryServer() throws Exception {
        if(!isMaryTTSInstalled()){
            notifyAllObservers("MaryTTS Server couldn't be found");
            throw new FileNotFoundException("MaryTTS Server couldn't be found");
        }
        if(isInstanceRunning() || serverStarted){
            System.out.println("\"Server already running");
            notifyAllObservers("Server already running");
           // throw new Exception("Server already running. Nothing to start");
            return false;
        }
        System.out.println("starting MaryTTS Srv");

        final String []command = buildMaryTTSCmd();
        final ProcessBuilder processB = new ProcessBuilder(command);
        processB.redirectErrorStream(true);
        Process p;
        try {
            executeAndNotify(processB);
        } catch (IOException e) {
            e.printStackTrace();
            notifyAllObservers("Server could not be started. Reason: " + e.getMessage());
            throw new Exception("MaryTTS Server could not be started");
        }
        System.out.println("ending starting MaryTTS Srv");

        return true;
    }

    private  synchronized void executeAndNotify(ProcessBuilder processB) throws IOException {
        Process p;
        p = processB.start();
        InputStream is = p.getInputStream();
        InputStreamReader isr = new InputStreamReader(is);
        BufferedReader br = new BufferedReader(isr);
        String line;
        boolean started = false;
        while (!started) {
            line = br.readLine();
            if (line != null && line.contains("started in")) {
                System.out.println("Line: " + line);
                started = true;
                serverStarted = true;
                is.close();
                isr.close();
                notifyAllObservers("Server started");
            }
        }
    }

    private String[] buildMaryTTSCmd(){
        String cmd = getMaryTTSExecPath();
        List<String> command = new LinkedList<>();
        if (isUnix() || isMac()) {
            command.add("/bin/bash");
            command.add(cmd);
        } else if (isWindows()) {
            cmd = cmd + ".bat";
            command.add("CMD");
            command.add("/C");
            command.add(cmd);
        }
        return (String[]) command.toArray(new String[command.size()]);
    }

    private boolean isMaryTTSInstalled(){
        String cmd = getMaryTTSExecPath();
        File f = new File(cmd);
        if(f.exists() && !f.isDirectory()) {
            return  true;
        }
        return false;
    }

    private String getMaryTTSExecPath(){
        final String maryttsBaseDir = maryBase;
        String cmd = maryttsBaseDir + File.separator + "bin" + File.separator + "marytts-server";
        return cmd;
    }

    private synchronized boolean isInstanceRunning(){
        Integer localPort = Integer.getInteger("server.port", 59125).intValue();
        try {
            ServerSocket serverSocket = new ServerSocket(localPort);
            serverSocket.close();
        } catch (IOException e) {
            return true;

        }

        return false;
    }

    public boolean stopMaryServer() throws IOException {
        try {
            if (isUnix() || isMac()) {
                killServerUnixMac();
            } else if (isWindows()) {
                killServerWindows();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.out.println("Could not kill the MaryTTS Server");
            return false;
        }finally {
            sInstance = null;
            serverStarted = false;
        }

        return true;
    }

    private void killServerUnixMac() throws IOException, InterruptedException {
        String killCmd = "";
        Process killer = null;
        killCmd = "ps aux | grep '" + instanceExecutedName + "' | awk '{print $2}' | xargs kill";
        String[] cmd = {"/bin/sh", "-c", killCmd};
        killer = Runtime.getRuntime().exec(cmd);
        killer.waitFor();

    }

    private void killServerWindows() throws IOException, InterruptedException {
        String killCmd = "";
        Process killer = null;
        killCmd = "wmic Path win32_process Where \"CommandLine Like '%" + instanceExecutedName + "%'\" Call Terminate";
        String[] cmd = {"/bin/sh", "-c", killCmd};
        killer = Runtime.getRuntime().exec(killCmd);
        killer.waitFor();

    }

    private boolean isWindows() {
        return (OS.indexOf("win") >= 0);
    }

    private boolean isMac() {
        return (OS.indexOf("mac") >= 0);
    }

    private boolean isUnix() {
        return (OS.indexOf("nix") >= 0 || OS.indexOf("nux") >= 0 || OS.indexOf("aix") > 0);
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
