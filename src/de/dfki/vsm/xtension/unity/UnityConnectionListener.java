package de.dfki.vsm.xtension.unity;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * @author J.-L. Himbert <s9jehimb@stud.uni-saarland.de>
 */
public class UnityConnectionListener extends Thread {
    private final LOGConsoleLogger _logger = LOGConsoleLogger.getInstance();
    private final UnityConnectionExecutor _executor;
    private final int _port;
    private ServerSocket _socket;
    private boolean _isListening;

    public UnityConnectionListener(final int port, final UnityConnectionExecutor executor) {
        _port = port;
        _executor = executor;
        _isListening = false;
    }

    @Override
    public final void start() {
        _isListening = true;
        try {
            _socket = new ServerSocket(_port);
            super.start();
        } catch (final IOException ex) {
            _logger.failure(ex.toString());
        }
    }

    public final void abort() {
        _isListening = false;
        if (_socket != null && !_socket.isClosed()) {
            try {
                _socket.close();
            } catch (final IOException ex) {
                _logger.failure(ex.toString());
            }
        }
        interrupt();
    }

    @Override
    public final void run() {
        while (_isListening) {
            try {
                final Socket socket = _socket.accept();
                _executor.accept(socket);
            } catch (final IOException ex) {
                _logger.failure(ex.toString());
            }
        }
    }
}