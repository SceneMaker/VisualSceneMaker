package de.dfki.vsm.xtension.unity;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.unity.commands.Command;
import de.dfki.vsm.xtension.unity.commands.RaiseHandCommand;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.net.Socket;

/**
 * @author J.-L. Himbert <s9jehimb@stud.uni-saarland.de>
 */
public class UnityConnectionHandler extends Thread {
    private final LOGConsoleLogger _logger = LOGConsoleLogger.getInstance();
    private final UnityConnectionExecutor _executor;
    private final Socket _socket;
    private boolean _done;
    private BufferedOutputStream _output;
    private BufferedInputStream _input;

    // Create the client thread
    public UnityConnectionHandler(final Socket socket, final UnityConnectionExecutor executor) {
        _socket = socket;
        _executor = executor;
        _done = false;
    }
    
    @Override
    public final void start() {        
        try {
            _output = new BufferedOutputStream(_socket.getOutputStream());
            _input = new BufferedInputStream(_socket.getInputStream());
        } catch (IOException ex) {
            _logger.message(ex.toString());
        }
        
        super.start();
    }

    // Abort the client thread
    public final void abort() {
        _done = true;
        if (_socket != null && !_socket.isClosed()) {
            try {
                _socket.close();
            } catch (final IOException ex) {
                _logger.failure(ex.toString());
            }
        }
        interrupt();
    }

    public final Command receiveNextCommand() {
        // IMPORTANT: We use unsigned bytes!
        try {
            if (_input.available() == 0) {
                return null;
            }
            
            final byte[] header = new byte[2];
            _input.read(header, 0, 2);
            
            final int payloadLength;
            if (((int) header[1] & 0xFF) < 255) {
                payloadLength = header[1];
            } else {
                final byte[] size = new byte[2];
                _input.read(size, 0, 2);
                payloadLength = new BigInteger(size).intValue();
            }
            final byte[] data = new byte[payloadLength];
            _input.read(data, 0, payloadLength);
            
            // TODO: Implement new commands here!
            Command command = new RaiseHandCommand(0, "");
            switch((int) header[0] & 0xFF) {
                case 1: {
                    // Speech Command -- Not for server?
                    // Maybe we use this as message command then...
                    break;
                }
                case 2: {
                    // Raise Hand Command -- Not for server?
                    break;
                }
                case 254: {
                    // Failure command?
                    _logger.warning("A failure command was received.");
                    return null;
                }
                case 255: {
                    // Aknowledged command?
                    _logger.success("A success command was received.");
                    return new RaiseHandCommand(0,"Test");
                }
                default: {
                    // Invalid command
                    _logger.failure("Received an invalid command: " + Byte.toString(header[0]));
                    return null;
                }
            }
            
            _logger.message("Received a new command: " + command.toString());
            return command;
        } catch (final Exception ex) {
            _logger.failure(ex.toString());
            return null;
        }
    }

    public final boolean send(final Command command) {
            final byte[] header;
            final byte[] data = command.GetData();
            if (data.length < 255) {
                header = new byte[] {command.getCode(), (byte)data.length};
            } else {
                header = new byte[] {command.getCode(), (byte)255, 
                                     (byte)(((short)data.length) & 0xff), 
                                     (byte)((((short)data.length) >> 8) & 0xff)};
            }
            
            final byte[] output = new byte[header.length + data.length];
            System.arraycopy(header, 0, output, 0, header.length);
            System.arraycopy(data, 0, output, header.length, data.length);
            
            try {
                _output.write(output);
                _output.flush();
                return true;
            } catch (final IOException ex) {
                _logger.failure(ex.toString());
                return false;
            }
    }

    @Override
    public final void run() {
        while (!_done) {
            final Command command = receiveNextCommand();
            if (command != null) {
                send(command);
                _executor.handle(command, this);
            }
        }
    }
}