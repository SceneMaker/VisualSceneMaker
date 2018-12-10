package de.dfki.vsm.util.xml;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.ByteArrayInputStream;
import org.w3c.dom.Document;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Properties;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * @author Gregor Mehlmann
 */
public final class XMLUtilities {

    // The singelton logger instance
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // Parse a parseable object from a string
    public final static boolean parseFromXMLString(final XMLParseable parsable, final String string, final String charset) {
        try {
            final ByteArrayInputStream stream = new ByteArrayInputStream(
                    string.getBytes(charset));
            //
            return parseFromXMLStream(parsable, stream);
        } catch (final UnsupportedEncodingException exc) {
            sLogger.failure(exc.toString());
        }
        //
        return false;
    }

    // Parse a parseable object from a stream
    public final static boolean parseFromXMLStream(final XMLParseable parsable, final InputStream stream) {
        try {
            // Construct the XML document parser
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            // Parse the XML document from the stream
            final Document document = parser.parse(stream);
            // Parse the parseable object from the document
            parsable.parseXML(document.getDocumentElement());
            // Finally close the stream and the URL
            stream.close();
            // Return true if parsing was successfull
            return true;
        } catch (final XMLParseError | IOException | ParserConfigurationException | SAXException exc) {
            // Print an error message in this case
            sLogger.failure(exc.toString());
            // Return failure if the parsing failed
            return false;
        }
    }

    // Parse java properties from an stream
    public final static boolean parseFromXMLStream(final Properties properties, final InputStream stream) {
        try {
            // Parse the java properties from stream
            properties.loadFromXML(stream);
            // Finally close the stream and the file
            stream.close();
            // Return true if parsing was successfull
            return true;
        } catch (final IOException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Parse a parseable object from an URL
    public final static boolean parseFromXMLURL(final XMLParseable parsable, final URL url) {
        try {
            // Open the URL with an input stream
            final InputStream stream = url.openStream();
            // Parse the parseable from the stream
            return parseFromXMLStream(parsable, stream);
        } catch (final IOException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if parsing XML failed
            return false;
        }
    }

    // Parse java properties from an URL
    public final static boolean parseFromXMLURL(final Properties properties, final URL url) {
        try {
            // Open the url with the input stream
            final InputStream stream = url.openStream();
            // Parse the parseable from the stream
            return parseFromXMLStream(properties, stream);
        } catch (final IOException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Parse a parseable object from a file
    public final static boolean parseFromXMLFile(final XMLParseable parsable, final File file) {
        try {
            // Open the file with a file input stream
            final FileInputStream stream = new FileInputStream(file);
            // Parse the parseable from the stream
            return parseFromXMLStream(parsable, stream);
        } catch (final FileNotFoundException exc) {
            // Print an error message in this case
            sLogger.failure(exc.toString());
            // Return failure if the parsing failed
            return false;
        }
    }

    // Parse java properties from a file
    public final static boolean parseFromXMLFile(final Properties properties, final File file) {
        try {
            // Open the file with the input stream
            final FileInputStream stream = new FileInputStream(file);
            // Parse the parseable from the stream
            return parseFromXMLStream(properties, stream);
        } catch (final FileNotFoundException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Write a writeable object to an indent writer
    public final static boolean writeToXMLWriter(final XMLWriteable writeable, final IOSIndentWriter writer) {
        try {
            // Write the XML header line to the stream
            writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            // Write the writeable object to the stream
            writeable.writeXML(writer);
            // Flush and close the writer and the stream
            writer.flush();
            writer.close();
            // Return true if writing was successfull
            return true;
        } catch (final XMLWriteError exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Write a writeable object to a file
    public final static boolean writeToXMLFile(final XMLWriteable writeable, final File file, final String charset) {
        try {
            // Open the file with an indent writer
            final IOSIndentWriter writer = new IOSIndentWriter(file, charset);
            // Write the writeable object to writer
            return writeToXMLWriter(writeable, writer);
        } catch (final IOException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Write a writeable object to a stream 
    public final static boolean writeToXMLStream(final XMLWriteable writeable, final OutputStream stream) {
        // Open the stream with an indent writer
        final IOSIndentWriter writer = new IOSIndentWriter(stream);
        // Write the writeable object to writer
        return writeToXMLWriter(writeable, writer);
    }

    // Write java properties to a stream
    public final static boolean writeToXMLStream(final Properties properties, final OutputStream stream) {
        try {
            // Write the java properties to the stream
            properties.storeToXML(stream, "", "UTF-8");
            // Flush and close the stream and the file
            stream.flush();
            stream.close();
            // Return true if writing was successfull
            return true;
        } catch (final IOException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    // Write java properties to a file 
    public final static boolean writeToXMLFile(final Properties properties, final File file) {
        try {
            // Open the file with the output stream
            final FileOutputStream stream = new FileOutputStream(file);
            // Write java properties to the stream
            return writeToXMLStream(properties, stream);
        } catch (final FileNotFoundException exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return false if writing to XML failed
            return false;
        }
    }

    public final static Document xmlStringToDocument(final String string) {
        try {
            final ByteArrayInputStream stream = new ByteArrayInputStream(
                    string.getBytes(/*"UTF-8"*/));
            // Construct the XML document parser
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            // Parse the XML document from the stream
            final Document document = parser.parse(stream);
            // Finally close the stream and the URL
            stream.close();
            //
            return document;
        } catch (final Exception/*ParserConfigurationException | SAXException | IOException */ exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return null if parsing to XML failed
            return null;
        }
    }

    public final static String xmlElementToString(final Element element) {
        try {
            final Transformer transformer
                    = TransformerFactory.newInstance().newTransformer();
            final StreamResult result
                    = new StreamResult(new StringWriter());
            final DOMSource source = new DOMSource(element);
            transformer.transform(source, result);
            return result.getWriter().toString();
        } catch (final TransformerException | TransformerFactoryConfigurationError exc) {
            // Print some error message in this case
            sLogger.failure(exc.toString());
            // Return null if parsing to XML failed
            return null;
        }
    }

    // Pretty print XML event
    public final static String xmlStringToPrettyXMLString(final String string) {
        try {
            //mLogger.warning("Pretty Printing '" + string + "'");
            // Get the string input stream
            final ByteArrayInputStream stream = new ByteArrayInputStream(
                    string.replaceAll(">\\s*<", "><").getBytes("UTF-8"));
            // Construct the XML pipeline
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            final Transformer transformer
                    = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            // Parse the XML document
            final StreamResult result
                    = new StreamResult(new StringWriter());
            final Document document = parser.parse(stream);
            final DOMSource source = new DOMSource(document);
            // Transform the document
            transformer.transform(source, result);
            // Return the representation
            return result.getWriter().toString();
        } catch (final ParserConfigurationException
                | TransformerException
                | IllegalArgumentException
                | SAXException
                | IOException exc) {
            sLogger.failure(exc.toString());
            // Return failure if the parsing failed
            return null;
        }
    }
}
