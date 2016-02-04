package de.dfki.vsm.util.syn;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Font;
import java.awt.Graphics;

import java.io.ByteArrayOutputStream;

import java.net.URL;

import java.util.HashMap;

import javax.swing.text.AttributeSet;
import javax.swing.text.Segment;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.TabExpander;
import javax.swing.text.Utilities;

/**
 * @author Not me
 */
public final class SyntaxStylePolicy implements ModelObject {

    // The Singelton Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Token Style Map
    private final HashMap<String, SyntaxTokenStyle> mStyleMap = new HashMap<>();

    // The Syntax Style URL
    final URL mURL;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SyntaxStylePolicy(final URL url) {
        // Initialize The URL
        mURL = url;
        // Parse The Policy
        try {
            // Parse The Policy From An URL
            if (XMLUtilities.parseFromXMLURL(this, mURL)) {

                // Print Some Debug Information
                mLogger.message("Success: Loading Style Policy URL:\n" + toString());
            } else {
                // Print Some Information
                mLogger.failure("Failure: Cannot Parse Style Policy URL '"
                        + mURL.toString() + "'");
            }
        } catch (Exception exc) {
            // Print Some Information
            mLogger.failure("Failure: Cannot Parse Style Policy URL '"
                    + mURL.toString() + "'");
            // Print Some Information
            mLogger.failure(exc.toString());
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final SyntaxTokenStyle getStyle(final String token) {

        // Get The Syntax Style Entry
        return mStyleMap.get(token);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int drawStyle(Segment segment, int x, int y, Graphics graphics, TabExpander e, int offset,
            final String token) {

        // Get The Syntax Style Entry
        final SyntaxTokenStyle entry = mStyleMap.get(token);

        // Set The Font Style Emph
        final Font font = graphics.getFont().deriveFont((entry.isEmph()
                ? Font.ITALIC
                : Font.PLAIN) | (entry.isBold()
                ? Font.BOLD
                : Font.PLAIN));

        // Set The New Font
        graphics.setFont(font);

        // Set The Foreground Color
        graphics.setColor(entry.getFCol());

        // Draw The Text Foreground
        return Utilities.drawTabbedText(segment, x, y, graphics, e, offset);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IOSIndentWriter writer) throws XMLWriteError {
        writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        writer.println("<StylePolicy>").push();

        for (SyntaxTokenStyle entry : mStyleMap.values()) {
            entry.writeXML(writer);
            writer.endl();
        }

        writer.pop().print("</StylePolicy>");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void parseXML(final Element element) throws XMLParseError {

        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, "TokenStyle", new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {

                // Create A New Token Style
                SyntaxTokenStyle entry = new SyntaxTokenStyle();

                // Parse The New Token Style
                entry.parseXML(element);

                try {

                    // Put The New Style To The Map
                    mStyleMap.put(entry.getName(), entry);
                } catch (Exception exc) {
                    mLogger.warning(exc.toString());
                }
            }
        });
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public SyntaxStylePolicy getCopy() {
        return new SyntaxStylePolicy(mURL);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {

        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);

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
