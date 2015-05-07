
/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
 */
package de.dfki.vsm.util.request;

/**
 * @author Gregor Mehlmann
 */
public class UDPClient extends Client {
    public UDPClient(ThreadGroup group, String host, int port, Request request, Crowd crowd) {
        super(group, host, port, request, crowd);
    }

    public void pleaseStop() {
        mStop = true;
        interrupt();    // Stop blocking in accept()

//      try {
//          //mSocket.close();
//      } catch (IOException e) {
//          e.printStackTrace();
//          mLogger.message(e.getMessage());
//      }
    }

    @Override
    public void run() {
        while (!mStop) {

//          try {
//
//          } catch (InterruptedIOException e) {
//              //e.printStackTrace();
//              mLogger.message("ServiceServer:" + e.getMessage());
//          } catch (IOException e) {
//              //e.printStackTrace();
//              mLogger.message("ServiceServer:" + e.getMessage());
//          }
        }
    }
}
