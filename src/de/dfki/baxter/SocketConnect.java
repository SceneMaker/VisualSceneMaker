package de.dfki.baxter;



import java.io.*;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Created by alvaro on 23.05.15.
 * Client to connect to baxter via tpc/ip
 */
public class SocketConnect {
    private static String tcp_ip = "127.0.0.1";
    private static int tcp_port = 1313;
    private static Socket socket = null;
    private static DataInputStream is = null;
    private static DataOutputStream os = null;
    private static SocketConnect instance  = new SocketConnect();
    public SocketConnect SocketConnect(){
        if(socket == null ){
           connect();
           
        }
        return this;

    }
    
    public static SocketConnect getInstance(){
        if(socket == null ){
            
            connect();
        }
          
         if(socket == null ){
             return null;
         }
        
        return instance;
    }

    public static void send(String data) {
        
        boolean result = true;
        String noReset = "Could not reset.";
        String reset = "The server has been reset.";
        
        try {
            is = new DataInputStream(socket.getInputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            os = new DataOutputStream(socket.getOutputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
        PrintWriter pw = new PrintWriter(os);
        pw.println(data);
        pw.flush();
        BufferedReader in = new BufferedReader(new InputStreamReader(is));
        String read_result;
        try {
                read_result = in.readLine();
                System.out.println("Resultado del server: " + read_result);
                
            
        } catch (IOException ex) {
            Logger.getLogger(SocketConnect.class.getName()).log(Level.SEVERE, null, ex);
        }
    
    }

    public static void connect()  {
        socket = null;
        try {
            socket = new Socket(InetAddress.getByName(tcp_ip), tcp_port);
        } catch (UnknownHostException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
    
    public static void close(){
        System.out.println("Closing connection");
        try {
            is.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        try {
            os.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        try {
            socket.close();
        } catch (IOException ex) {
            Logger.getLogger(SocketConnect.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
