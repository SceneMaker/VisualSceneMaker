package de.dfki.vsm.xtension.remote.server.parsers.xml;


import de.dfki.vsm.xtension.remote.server.commands.NotifiableCommand;
import de.dfki.vsm.xtension.remote.server.parsers.Parser;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.InvalidValue;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.NoValueProvided;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.util.LinkedList;

/**
 * Created by alvaro on 5/10/17.
 */
public abstract class ResponseParser implements Parser {
    public static final String VALUE_TAG = "value";
    protected XMLReader reader;
    protected NotifiableCommand notification;
    protected LinkedList<String> values = new LinkedList<>();

    public ResponseParser(String data) throws ParserConfigurationException, SAXException, IOException {
        reader = new XMLReader(data);
    }

    @Override
    public abstract NotifiableCommand parse() throws InvalidValue, NoValueProvided;

    protected LinkedList<String> getAllValues() throws NoValueProvided {
        values =  reader.getAllTagsValues(VALUE_TAG);
        shouldReturnValues();
        return values;
    }

    private void shouldReturnValues() throws NoValueProvided {
        if(values.size() <= 0 ){
            throw new NoValueProvided("No value tag provided");
        }
    }
}
