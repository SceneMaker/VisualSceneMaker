/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.baxter;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Logger;

/**
 *
 * @author alvaro
 */
public class BaxterPlayer {
    private String variable;
     public  static BaxterPlayer sInstance = null;
     private static final Logger log = Logger.getLogger( "CharamelLogger");
     private static SocketConnect connection ;
     private final PlayerConfig mPlayerConfig;
     
    public BaxterPlayer(PlayerConfig playerConfig){
        this.variable = "Hello Baxter";
        mPlayerConfig = playerConfig;
        String lhost = mPlayerConfig.getProperty("vsm.agent.remote.host");
        String lport = mPlayerConfig.getProperty("vsm.agent.remote.port");
        
        connection = new SocketConnect();
        if(lhost != null){
           SocketConnect.setIp(lhost);
        }
        if(lport != null){
            SocketConnect.setPort(Integer.parseInt(lport));
        }
    }
    
    public BaxterPlayer(){
        this.variable = "Hello Baxter";
        mPlayerConfig = null;
        connection = new SocketConnect();
    }
    
    public void closeBaxterConnection(){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"closeConnection\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println("Closing socket");
        SocketConnect.close();
    }
    
    public static void lookAtPoint (String character, int x, int y, int z, int timer){
        System.out.println(":)");
    }
    
    public static void setLanguage(String character, String msg){
        System.out.println(character + " said: " + msg);
        
    }
    
    public static void logToSocket(String text){
        log.info(text);
        System.out.println("HOLA");
        
    }
    
    public static void doBye(String character) {
        //Todo implement joint movements
        System.out.println(character + " say: Bye" );
    }
    

    public static void doWave(String character) {
        //Todo implement joint movements
        System.out.println(character + " say: Hello" );
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"wave\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
                
    }

    public static void doSmile(String character){
        System.out.println(character + " is smiling 1" );
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"smile\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is smiling" );

    }
    
    public static void doAngry(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"frown\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is angry" );

    }
    
    public static void doBored(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"bored\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is bored" );

    }
    
   
    public static void doNodassent(String character){
        if(SocketConnect.getInstance() != null){
            String smile_command = "{\"method\":\"assent\",\"params\":\"null\"}\n";
            SocketConnect.send(smile_command);
        }
        System.out.println(character + " is assenting" );
    }
    
    public static void doHandshake(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"handshake\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is handshaking" );
    }
    
    public static void doCome(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"come\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is calling you to come" );
    }
    
    public static void doPointright(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"pointRight\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is pointing right" );
    }

    public static void doTest(String character){
        if(SocketConnect.getInstance() != null){
            String command = "{\"method\":\"test\",\"params\":\"null\"}\n";
            SocketConnect.send(command);
        }
        System.out.println(character + " is test" );
    }
    
    public static void doNodnegate(String character){
        System.out.println(character + " is negatting" );
    }
    
    @Override
    protected void finalize() throws Throwable {
        System.out.println("In finalize block");
        SocketConnect.close();
    }
    
}
