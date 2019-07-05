package de.dfki.vsm.xtension.pepper;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.*;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;


public class PepperExecutor extends ActivityExecutor implements ExportableProperties  {

    // strange pattern: we have to both aggregate _and_ implement ExportableProperties for them to show up in the settings...
    private PepperExtensionProperties extensionProperties = new PepperExtensionProperties();

    private ServerSocket serverSocket = null;
    private Socket clientSocket = null;
    // separate thread for reading; writing can be done from the main thread
    private Thread clientConnection = null;
    private BlockingQueue<String> clientMessages = new LinkedBlockingQueue<>();

    private PrintWriter pepperOut = null;

    public PepperExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);

        // TODO read project properties

//        mLogger.message("Printing project settings:");
//        for (Map.Entry<ProjectProperty, ProjectValueProperty> p : extensionProperties.getExportableProperties().entrySet()) {
//            mLogger.message("Entry: " + p.getKey() + ": " + p.getValue().getValue());
//        }
        mLogger.message("mConfig Entry: " + mConfig.getProperty("Python 2.7 (32 bit) executable"));
        mLogger.message("mConfig Entry: " + mConfig.getProperty("Pepper IP address", "pepper.local"));
        // TODO investigate: if I don't specify the defaultValue null is returned;
        // why do I need to specify it _again_ when I already did so in the definition of PepperExtensionProperties?
        // whole plugin property architecture seems convoluted...

        // mConfig inherited from RunTimePlugin via ActivityExecutor
        //mConfig.getProperty("myProperty", "default");
        // TODO are the properties validated, or do we have to do it? (I think not, there's a function that always returns true)

        // mLogger inherited from RunTimePlugin via ActivityExecutor
        //mLogger.message("Constructing PepperExecutor");
        // logger can be accessed via LOGConsoleLogger.getInstance()
    }

    @Override
    public String marker(long id) {
        return "$" + id;
    }

    @Override
    public void execute(AbstractActivity activity) {
        //mLogger.message("In execute: " + System.getProperty("user.dir"));
        if (activity instanceof SpeechActivity) {
            System.out.println(activity.getActor() + ": " + activity.getText());
            pepperOut.println(activity.getText());
        }

        // send text to python process here
    }

    @Override
    public void launch() {
        /**
         * Create server socket in separate thread, because
         * - the server socket has to be started before the Python client process but
         * - the server socket blocks execution of its thread while it is listening
         */
        ConnectionCreator creator = new ConnectionCreator(12345, clientMessages);
        creator.start();

        /**
         * Start the Python process and let it connect to this program
         */
        String pythonScript = "Pepper.py";
        String pythonExecutable = mConfig.getProperty("Python 2.7 (32 bit) executable");
        String pepperIPAddress = mConfig.getProperty("Pepper IP address", "pepper.local");
        ProcessBuilder processBuilder = new ProcessBuilder(pythonExecutable, pythonScript,
                "--address-pepper", pepperIPAddress);


        String pepperExecutorDir = getPepperExecutorDirectory();
        
        processBuilder.directory(new File(pepperExecutorDir));
        processBuilder.inheritIO();
        Process process = null;
        try {
            process = processBuilder.start();
        } catch (IOException e) {
            e.printStackTrace();
        }

        /**
         * Wait until the connection is established
         */
        try {
            creator.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        // the server has to listen to incoming connections concurrently, but the assigned socket objects have
        // to be communicated back to the main thread - is there a better way than this?
        serverSocket = creator.getServerSocket();
        clientSocket = creator.getClientSocket();
        clientConnection = creator.getClientConnection();

        try {
            pepperOut = new PrintWriter(clientSocket.getOutputStream(), true);
        } catch (IOException e) {
            e.printStackTrace();
        }

        /*
        // start external dependencies and initiate connections
        System.out.println("In launch: " + System.getProperty("user.dir"));

        ProcessBuilder processBuilder = new ProcessBuilder("cmd", "/c", python2Path, pythonScript);
        try {
            pythonProcess = processBuilder.start();
//            BufferedReader reader = new BufferedReader(new InputStreamReader(pythonProcess.getInputStream()));
//            System.out.println("Python says: " + reader.readLine());
        } catch (IOException e) {
            e.printStackTrace();
        }

        try {
            serverSocket = new ServerSocket(12345);
            clientSocket = serverSocket.accept();
            System.out.println("Client has connected");
            clientConnection = new SocketReader(clientSocket, clientMessages);
            clientConnection.start();
            pepperOut = new PrintWriter(clientSocket.getOutputStream(), true);
        } catch (IOException e) {
            e.printStackTrace();
        }
        */
    }

    @Override
    public void unload() {
        // terminate all connections and free resources
        pepperOut.close();
        try {
            clientSocket.close();
            serverSocket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Hacky way to get the directory containing the PepperExecutor source file,
     * which contains the needed "Pepper.py" Python script.
     *
     * Assumes that the Visual Scene Maker is executed via a .jar located three
     * directory levels deep in a folder which also contains the VSM source.
     */
    private String getPepperExecutorDirectory() {
        String VSMjarPath = PepperExecutor.class.getProtectionDomain().getCodeSource().getLocation().toString();
//        mLogger.message(VSMjarPath);
        String[] splitPath = VSMjarPath.split("/");
        // now remove "file:" prefix and trailing relative path from VSM base directory to jar (something like "build\libs\VisualSceneMaker-4.0.0.jar")
        String VSMbasePath = String.join("/", Arrays.copyOfRange(splitPath, 1, splitPath.length - 3));
        String pepperExecutorPath = VSMbasePath.concat("/src/main/java/de/dfki/vsm/xtension/pepper");
//        mLogger.message(pepperExecutorPath);
        return pepperExecutorPath;
    }

    /**
     * Reads (newline-terminated) messages in blocking mode from the given socket and puts them in
     * the (tread-safe) queue, from which other threads can then consume them.
     */
    public static class SocketReader extends Thread {

        private Socket socket;
        private BlockingQueue<String> messageOutQueue;
        private BufferedReader socketReader;

        private static int idCounter = 0;

        public SocketReader(Socket socket, BlockingQueue<String> messageOutQueue)
                throws IOException {
            this.socket = socket;
            this.messageOutQueue = messageOutQueue;
            this.socketReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            this.setName("SocketReader-" + idCounter++);
        }

        @Override
        public void run() {
            try {
                while (true) {
                    // todo readLine throws exception when socket is closed, handle to terminate gracefully
                    String newMessage = socketReader.readLine();
                    if (newMessage == null) {  // EOF read
                        socketReader.close();
                        socket.close();
                        /* we need some mechanism to signal to the outer (main) thread that
                         * the connection was terminated;
                         * here I add a "stop word" to the message queue
                         * alternatives: shared flag variable (thread-safe!) that is checked from outer thread
                         * and using poll with timeout instead of take
                         * see also https://stackoverflow.com/questions/812342/how-to-interrupt-a-blockingqueue-which-is-blocking-on-take
                         */
                        messageOutQueue.add("disconnected");
                        break;
                    }
                    messageOutQueue.add(newMessage);
                }
            } catch (IOException e) {
                // log error
                e.printStackTrace();
            }
        }
    }

    /**
     * Establish a connection to the client and start listener thread.
     *
     * Once the connection is established, the objects associated with the connection
     * can be retrieved using the get methods.
     */
    public static class ConnectionCreator {
        int port;

        Socket clientSocket;
        ServerSocket serverSocket;
        Thread clientConnection;
        BlockingQueue<String> clientMessages;

        Thread creatorThread;

        /**
         * @param port Port at which the server socket will accept incoming connections
         * @param clientMessages Synchonized queue from which client messages can be accessed once the connection is established
         */
        public ConnectionCreator(int port, BlockingQueue<String> clientMessages) {
            this.port = port;
            this.clientMessages = clientMessages;
//            this.creatorThread = new Thread() {
//                public void run() {
//                    try {
//                        serverSocket = new ServerSocket(port);
//                        clientSocket = serverSocket.accept();
//                        System.out.println("Client has connected");
//                        clientConnection = new SocketReader(clientSocket, clientMessages);
//                        clientConnection.start();
//                    } catch (IOException e) {
//                        e.printStackTrace();
//                    }
//                }
//            };
            this.creatorThread = new Thread( () -> {
                try {
                    serverSocket = new ServerSocket(port);
                    clientSocket = serverSocket.accept();
//                    System.out.println("Client has connected");
                    LOGConsoleLogger.getInstance().message("Client has connected");
                    clientConnection = new SocketReader(clientSocket, clientMessages);
                    clientConnection.start();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
        }

        public void start() {
            creatorThread.start();
        }

        public boolean isAlive() {
            return creatorThread.isAlive();
        }

        public void join() throws InterruptedException {
            creatorThread.join();
        }

        public Thread getClientConnection() {
            return clientConnection;
        }

        public Socket getClientSocket() {
            return clientSocket;
        }

        public ServerSocket getServerSocket() {
            return serverSocket;
        }
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return extensionProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return null;
    }
}