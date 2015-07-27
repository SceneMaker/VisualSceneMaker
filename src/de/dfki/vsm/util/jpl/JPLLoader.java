package de.dfki.vsm.util.jpl;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

/**
 * @author Not me
 */
public class JPLLoader extends Thread {

    // The System File Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The Prolog Source File
    private final String mSource;

    // Initialize The Loader
    public JPLLoader(final String source) {

        // Give The Thread A Name
        super("JPLLoader(" + source + ")");

        // Initialize The Source
        mSource = source;
    }

    // Execute The JPL Loader
    @Override
    public void run() {

        // Call The Query To The Engine
        final JPLResult result = JPLEngine.query("consult('" + mSource + "').");

        // Print Debug Information
        mLogger.message("Consulted Source '" + mSource + "' In JPL Engine '" + JPLEngine.string() + "'");
    }
}
