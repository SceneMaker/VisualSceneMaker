package de.dfki.vsm.xtension.remotesender.sender;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by alvaro on 5/2/17.
 */
public interface Clientable {
    void setDataCreator(DataSendable dataCreator);

    void connect() throws IOException;
    void send() throws IOException;

    InputStream getInputStream() throws IOException;

}
