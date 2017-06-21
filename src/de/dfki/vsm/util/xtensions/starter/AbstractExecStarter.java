package de.dfki.vsm.util.xtensions.starter;


import de.dfki.vsm.util.xtensions.observers.Observable;

import java.io.File;
import java.io.IOException;

/**
 * Created by alvaro on 6/20/17.
 */
public abstract class AbstractExecStarter implements ExecutableStarter, Observable {

    private final String applicationPath;
    protected boolean applicationStarted = false;
    protected boolean onlyOneInstanceAllowed = false;
    protected boolean isApplicationStarted = false;
    protected String applicationName = "";

    public AbstractExecStarter(String applicationPath, String applicationName){
        this.applicationPath  = applicationPath;
        this.applicationName = applicationName;
    }
    @Override
    public boolean executeApplication()  {
        if(!execExists()){
            notifyAll("Executable cannot be found");
            return false;
        }
        if(onlyOneInstanceAllowed() && applicationAlreadyStarted()){
            notifyAll("Application already running");
            return false;
        }

        System.out.println("Starting application...");
        this.startApplication();
        return false;
    }

    private void startApplication() {
        CommandBuilder commandBuilder = CommandBuilder.getCommandBuilder(applicationPath, applicationName);
        Executor executor = new Executor(commandBuilder);
        try {
            start(executor);
        } catch (IOException e) {
            notifyError(e.getMessage());
        }

    }

    private void start(Executor executor) throws IOException {
        Process p = executor.execute();
        if(p!=null){
            Notifier iStreamNotifier = new CmdStreamNotifier(p.getInputStream(), "started in");
            boolean started = iStreamNotifier.waitForStartedNotification();
            makeStartedNotification(started);
        }
    }

    private void makeStartedNotification(boolean started) {
        if(started){
            applicationStarted = true;
            notifySuccess("Server started");
        }else{
            notifyError("");
        }
    }

    private void notifyError(String reason) {
        notifyAll("Server could not be started: " + reason);
    }

    private void notifySuccess(String message) {
        notifyAll(message);
    }


    @Override
    public boolean execExists(){
        File f = new File(applicationPath);
        return  f.exists() && !f.isDirectory();
    }

    public boolean onlyOneInstanceAllowed(){
        return onlyOneInstanceAllowed;
    }
    public  boolean applicationAlreadyStarted(){
        return isApplicationStarted;
    }


}
