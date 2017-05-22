package de.dfki.vsm.xtension.remotesender.server.factories;

import de.dfki.vsm.xtension.remotesender.server.parsers.Parser;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.XMLReader;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.exceptions.NoTagFound;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

/**
 * Created by alvaro on 5/10/17.
 */
public abstract class ParserFactory {

    public static final String INFORMATION_TAG_NAME = "messages";
    public static final String TYPE_ATTRIBUTE = "type";

    protected String data;
    protected Parser parser;
    protected XMLReader reader;
    protected abstract Parser buildParser(String data);

    public Parser createParser(String data) {
        this.data = data;
        String type = getXMLParser(data);
        parser = buildParser(type);
        return parser;
    }

    private String getXMLParser(String data)  {
        try {
            return getXMLParserType(data);
        } catch (IOException | ParserConfigurationException | SAXException | NoTagFound e) {
            e.printStackTrace();
            return "Dummy";
        }

    }

    private String getXMLParserType(String data) throws IOException, ParserConfigurationException, SAXException, NoTagFound {
        String messageType ;
        reader = new XMLReader(data);
        Element messages = reader.getFirstTag(INFORMATION_TAG_NAME);
        messageType = messages.getAttribute(TYPE_ATTRIBUTE);
        return messageType;
    }


}
