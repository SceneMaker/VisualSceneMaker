package de.dfki.vsm.xtension.reeti;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Robbie
 *
 * 1 Record the value of the variable in the command. For example, String test =
 * "10". [motor neckPan=test smooth='0.6'] ReetiCommandUtility stores
 * ("test","10") in HashMap, and updates the command to [motor neckPan='10'
 * smooth='0.6']. setCommandValue() should be called beofre we send to commnad
 * out.
 *
 * 2 Receive the feedback from opencv to adjust the action of reeti
 *
 */
public class ReetiCommandUtility {

    // used to adjust the value of command.
    private static HashMap<String, String> valRecord = new HashMap<String, String>();

    // Receive the feedback from opencv to adjust the action of reeti
    private static DatagramSocket serverSocket;
    private static String feedback = "unknown";
    private static Boolean UDPrunning = false;

    public static void setCommandValue(String key, String value) {
        valRecord.put(key, value);
        System.out.println("HashMapvalRecord: " + "key:" + key + "   value: " + valRecord.get(key));
    }

    public static void setCommandValue(String key, Object value) {
        if (value instanceof String) {
            String s = (String) value;
            valRecord.put(key, s);
            System.out.println("HashMapvalRecord: " + "key:" + key + "   value: " + valRecord.get(key));
        } else if (value instanceof Integer) {
            String svalue = String.valueOf(value);
            valRecord.put(key, svalue);
            System.out.println("HashMapvalRecord: " + "key:" + key + "   value: " + valRecord.get(key));
        }
    }

    public static Boolean checkCommandValue(String key) {
        return valRecord.containsKey(key);
    }

    public static String updateCommandValue(String key) {
        return valRecord.get(key);
    }

    // used to receive the feedback from opencv to adjust the action of reeti
    public static void receiveOpenCVFeedback() throws IOException {
        feedback = "unknown";
        if (!UDPrunning) {
            serverSocket = new DatagramSocket(9876);
            UDPrunning = true;
            while (true) {
                byte[] receiveData = new byte[5];
                DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);
                serverSocket.receive(receivePacket);
                feedback = new String(receivePacket.getData());
                System.out.println("RECEIVED: " + feedback);
//                  InetAddress IPAddress = receivePacket.getAddress();
//                  int port = receivePacket.getPort();
//                  String capitalizedSentence = sentence.toUpperCase();
//                  sendData = capitalizedSentence.getBytes();
//                  DatagramPacket sendPacket =
//                  new DatagramPacket(sendData, sendData.length, IPAddress, port);
//                  serverSocket.send(sendPacket);
//                  serverSocket.close();
            }
        }
    }

    // get OpenCV Feedback
    public static String getOpenCVFeedback() {
        feedback = "unknown";
        try {
            Thread.sleep(200);
        } catch (InterruptedException ex) {
            Logger.getLogger(ReetiCommandUtility.class.getName()).log(Level.SEVERE, null, ex);
        }
        if (feedback.contains("left")) {
            return "left";
        } else if (feedback.contains("mid")) {
            return "mid";
        } else if (feedback.contains("right")) {
            return "right";
        } else {
            return "unknown";
        }
    }

    // get current position of neckRotate motor
    public static int getCurrentPosition() {
        return ReetiExecutor.getCurrentPosition();
    }
}
