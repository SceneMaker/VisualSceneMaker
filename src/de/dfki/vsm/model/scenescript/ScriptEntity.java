package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public abstract class ScriptEntity implements ModelObject {

    // The System Logger
    protected LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    @Override
    public final String toString() {
        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);
        try {
            // Write Object
            writeXML(stream);
        } catch (final XMLWriteError exc) {
            mLogger.failure(exc.toString());
        }
        // Cleanup Stream and Writer
        stream.flush();
        stream.close();
        // Return String Representation
        try {
            //return buffer.toString("UTF-8");
            return buffer.toString();
        } catch (final Exception exc) {
            exc.printStackTrace();
            //
            return null;
        }
    }

    // The Lower Bound
    protected int mLower;
    // The Upper Bound
    protected int mUpper;

    public ScriptEntity() {
    }

    public ScriptEntity(final int lower, final int upper) {
        // Initialize Boundary
        mLower = lower;
        mUpper = upper;
    }

    public final int getLower() {
        return mLower;
    }

    public final int getUpper() {
        return mUpper;
    }

    public abstract String getText();

    public abstract String getText(final HashMap<String, String> args);

    @Override
    public abstract ScriptEntity getCopy();
}
