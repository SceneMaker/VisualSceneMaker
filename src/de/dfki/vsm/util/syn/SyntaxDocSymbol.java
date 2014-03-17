package de.dfki.vsm.util.syn;

import de.dfki.vsm.util.ios.IndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.io.ByteArrayOutputStream;
import java_cup.runtime.Symbol;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public class SyntaxDocSymbol extends Symbol implements ModelObject {

    // The System Logger
    private final LOGDefaultLogger mLogger
            = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SyntaxDocSymbol(
            final int field,
            final int lower,
            final int upper,
            final SyntaxDocToken token) {
        super(field, lower, upper, token);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final SyntaxDocToken getValue() {
        return (SyntaxDocToken) value;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void writeXML(final IndentWriter stream) throws XMLWriteError {
        stream.println("<SyntaxDocSymbol>");
        stream.push();
        ((SyntaxDocToken) value).writeXML(stream);
        stream.endl();
        stream.pop();
        stream.print("</SyntaxDocSymbol>");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void parseXML(final Element element) throws XMLParseError {
        // TODO: Implement
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public SyntaxDocSymbol getCopy() {
        // Recursively Get A Deep Copy
        return new SyntaxDocSymbol(sym, left, right,
                (SyntaxDocToken) ((SyntaxDocToken) value).getCopy());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IndentWriter stream = new IndentWriter(buffer);
        try {
            // Write Object
            writeXML(stream);
        } catch (XMLWriteError exc) {
            mLogger.failure(exc.toString());
        }
        // Cleanup Stream and Writer
        stream.flush();
        stream.close();
        // Return String Representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }

}
