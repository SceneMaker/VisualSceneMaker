package de.dfki.vsm.xtension.remote.server.receiver;



import de.dfki.vsm.xtension.remote.server.factories.ParserFactory;
import de.dfki.vsm.xtension.remote.server.commands.NotifiableCommand;
import de.dfki.vsm.xtension.remote.server.parsers.Parser;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.InvalidValue;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.NoValueProvided;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

/**
 * Created by alvaro on 4/28/17.
 */
public class DataReceiver implements Receiver {
    private ParserFactory parserFactory;
    private Parser parser;



    public DataReceiver(ParserFactory factory){
        this.parserFactory = factory;
    }

    @Override
    public void receive(String data) {
        try {
            processReceivedData(data);
        } catch (InvalidValue | NoValueProvided | ParserConfigurationException | SAXException | IOException invalidValue) {
            invalidValue.printStackTrace();
        }
    }

    private void processReceivedData(String data) throws InvalidValue, NoValueProvided, ParserConfigurationException, SAXException, IOException {
        parser = parserFactory.createParser(data);
        NotifiableCommand notification = parser.parse();
        notification.execute();
    }


}
