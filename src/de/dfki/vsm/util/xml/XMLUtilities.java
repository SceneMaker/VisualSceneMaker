package de.dfki.vsm.util.xml;

import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.w3c.dom.Document;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

/**
 * @author Gregor Mehlmann
 */
public final class XMLUtilities {

    // The Singelton Logger
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean parseFromXMLURL(final XMLParseable parsable, final URL url) {
        try {

            // Open The Resource URL
            final InputStream stream = url.openStream();

            // Construct The Parser
            final DocumentBuilder parser = DocumentBuilderFactory.newInstance().newDocumentBuilder();

            // Parse The Document
            final Document document = parser.parse(stream);

            // Parse The Model Object
            parsable.parseXML(document.getDocumentElement());

            // Close The File Stream
            stream.close();

            // Return True At Success
            return true;
        } catch (Exception exc) {

            // Print Some Information
            sLogger.failure(exc.toString());

            // Return False At Failure
            return true;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean parseFromXMLFile(final XMLParseable parsable, final File file) {
        try {
            // Open the file with a file input stream
            final FileInputStream stream = new FileInputStream(file);
            // Construct the document builder parser
            final DocumentBuilder parser
                    = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            // Parse The Document from the stream
            final Document document = parser.parse(stream);
            // Let the parsable parse itself now
            parsable.parseXML(document.getDocumentElement());
            // Close the file input stream then
            stream.close();
            // Return True At Success
            return true;
        } catch (final Exception exc) {
            // Print an error message in this case
            sLogger.failure(exc.toString());
            // Return failure if the parsing failed
            return false;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean writeToXMLFile(final XMLWriteable writeable, final File file) {
        try {

            // Open The Resource URL
            final IndentWriter stream = new IndentWriter(file);

            // Write The XML Header
            stream.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");

            // Write The Document
            writeable.writeXML(stream);

            // Close The File Stream
            stream.close();

            // Return True At Success
            return true;
        } catch (Exception exc) {

            // Print Some Information
            sLogger.failure(exc.toString());

            // Return False At Failure
            return true;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean parseFromXMLURL(final Properties properties, final URL url) {
        try {

            // Open The Resource URL
            final InputStream stream = url.openStream();

            // Load The Properties
            properties.loadFromXML(stream);

            // Close The File Stream
            stream.close();

            // Return True At Success
            return true;
        } catch (Exception exc) {

            // Print Some Information
            sLogger.failure(exc.toString());

            // Return False At Failure
            return true;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean parseFromXMLFile(final Properties properties, final File file) {
        try {

            // Open The Resource URL
            final FileInputStream stream = new FileInputStream(file);

            // Load The Properties
            properties.loadFromXML(stream);

            // Close The File Stream
            stream.close();

            // Return True At Success
            return true;
        } catch (Exception exc) {

            // Print Some Information
            sLogger.failure(exc.toString());

            // Return False At Failure
            return true;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static boolean writeToXMLFile(final Properties properties, final File file) {
        try {

            // Open The Resource URL
            final FileOutputStream stream = new FileOutputStream(file);

            // Write The Document
            properties.storeToXML(stream, "", "UTF-8");

            // Close The File Stream
            stream.close();

            // Return True At Success
            return true;
        } catch (Exception exc) {

            // Print Some Information
            sLogger.failure(exc.toString());

            // Return False At Failure
            return true;
        }
    }
}
