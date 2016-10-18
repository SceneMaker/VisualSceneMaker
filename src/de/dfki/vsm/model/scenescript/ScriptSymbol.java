package de.dfki.vsm.model.scenescript;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.syn.SyntaxDocSymbol;
import de.dfki.vsm.util.syn.SyntaxDocToken;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.lang.reflect.Field;

/**
 * @author Gregor Mehlmann
 */
public class ScriptSymbol extends SyntaxDocSymbol implements ScriptFields {

    // The System Logger
    public final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Array With Field Names
    public final static String[] sFieldNames;

    // The Array With Field Names
    public final static String[] sStateNames;

    // Initialize The Field Names
    static {
        Class   clazz  = ScriptFields.class;
        Field[] fields = clazz.getFields();

        // Fill The Names Array
        sFieldNames = new String[fields.length];

        for (final Field field : fields) {
            try {
                final int    value = field.getInt(null);
                final String name  = field.getName();

                // Set The Name Value
                sFieldNames[value] = name;
            } catch (IllegalAccessException | IllegalArgumentException exc) {

                // Do Nothing
            }
        }

        // Initialize The State Names
        clazz  = ScriptLexxer.class;
        fields = clazz.getFields();

        // Fill The Names Array
        sStateNames = new String[fields.length * 2];

        for (final Field field : fields) {
            try {
                final int    value = field.getInt(null);
                final String name  = field.getName();

                // Check If This Is A State (ex. EOF)
                if ((value >= 0) && name.startsWith("YY")) {

                    // Set The Name Value
                    sStateNames[value] = name;
                }
            } catch (IllegalAccessException | IllegalArgumentException exc) {

                // Do Nothing
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public ScriptSymbol(final int field, final int lower, final int upper, final SyntaxDocToken token) {
        super(field, lower, upper, token);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String getField(final int value) {
        return sFieldNames[value];
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String getState(final int value) {
        return sStateNames[value];
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<ScriptSymbol>");
        stream.push();
        ((SyntaxDocToken) value).writeXML(stream);
        stream.endl();
        stream.pop();
        stream.println("</ScriptSymbol>");
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void parseXML(Element element) throws XMLParseError {

        // TODO: Implement
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final ScriptSymbol getCopy() {

        // Recursively Get A Deep Copy
        return new ScriptSymbol(sym, left, right, (SyntaxDocToken) ((SyntaxDocToken) value).getCopy());
    }
}
