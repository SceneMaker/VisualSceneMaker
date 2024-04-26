package de.dfki.vsm.xtension.sockets;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;

public class DataSocket extends AbsJavaSocket{

    VSMSocketDataHandler dataExecutor;
    DatagramSocket socket;
    public DataSocket(VSMSocketDataHandler executor, int port) {
        super(executor, port);
        connect();
        dataExecutor = executor;
    }

    public DataSocket(VSMSocketDataHandler executor, String host, int port) {
        super(executor, host, port);
        connect();
        dataExecutor = executor;
    }
    public void run() {
        while (!done){

            byte[] receiveData = new byte[8]; // Assuming 8 bytes for a double value
            DatagramPacket receivePacket = new DatagramPacket(receiveData, receiveData.length);

            // Receive the DatagramPacket
            try {
                socket.receive(receivePacket);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            System.out.println("Received package: " + receivePacket.toString());
            // Extract the received data and convert it to a double
            double receivedDouble = byteArrayToDouble(receivePacket.getData());
            System.out.println("Received double value: " + receivedDouble);
            dataExecutor.handle(receivedDouble);

        }

    }


        private static double byteArrayToDouble(byte[] bytes) {
            if (bytes.length != 8) {
                throw new IllegalArgumentException("Byte array must have length 8 for a double value");
            }

            long longBits = ((long)bytes[0] & 0xFF) |
                    (((long)bytes[1] & 0xFF) << 8) |
                    (((long)bytes[2] & 0xFF) << 16) |
                    (((long)bytes[3] & 0xFF) << 24) |
                    (((long)bytes[4] & 0xFF) << 32) |
                    (((long)bytes[5] & 0xFF) << 40) |
                    (((long)bytes[6] & 0xFF) << 48) |
                    (((long)bytes[7] & 0xFF) << 56);

            return Double.longBitsToDouble(longBits);
        }

    @Override
    public void abort() {
socket.close();
    }

    @Override
    public void connect() {

        try {
            socket = new DatagramSocket(port);
        } catch (SocketException e) {
            throw new RuntimeException(e);
        }
    }
}