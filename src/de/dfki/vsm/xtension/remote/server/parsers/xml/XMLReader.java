package de.dfki.vsm.xtension.remote.server.parsers.xml;


import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.NoTagFound;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.NoValueProvided;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.LinkedList;

/**
 * Created by alvaro on 4/30/17.
 */
public class XMLReader {
    protected DocumentBuilder builder;
    ByteArrayInputStream byteData;
    private Element root;
    private Document doc;
    private LinkedList<String> valuesOfTag = new LinkedList<>();

    public XMLReader(String data) throws IOException, ParserConfigurationException, SAXException {
        this.byteData = new ByteArrayInputStream(data.getBytes("UTF-8"));
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        builder = factory.newDocumentBuilder();
        read();

    }

    public Element getFirstTag(String tagName) throws IOException, SAXException, NoTagFound {
        NodeList elements = getNodeListFrom(tagName);
        shouldFindTag(elements);
        return (Element) elements.item(0);
    }

    private NodeList getNodeListFrom(String tagName) {
        return doc.getElementsByTagName(tagName);
    }

    private void shouldFindTag(NodeList elements) throws NoTagFound {
        if (elements.getLength() <= 0) {
            throw new NoTagFound("No tag found with this name");
        }
    }

    private void read() throws SAXException, IOException {
        doc = builder.parse(byteData);
        root = doc.getDocumentElement();
    }

    public LinkedList<String> getAllTagsValues(String tagName) throws NoValueProvided {
        NodeList elements = getNodeListFrom(tagName);
        for (int i = 0; i < elements.getLength(); i++) {
            Element element = (Element) elements.item(i);
            shouldHaveValue(element);
            String value = element.getFirstChild().getNodeValue();
            valuesOfTag.add(value);
        }
        return valuesOfTag;
    }

    private void shouldHaveValue(Element element) throws NoValueProvided {
        if (element.getFirstChild() == null) {
            throw new NoValueProvided("This value tag does not have a value assigned");
        }
    }
}
