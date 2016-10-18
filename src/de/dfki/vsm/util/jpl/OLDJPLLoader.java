//package de.dfki.vsm.util.jpl;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//
///**
// * @author Gregor Mehlmann
// */
//public class OLDJPLLoader extends Thread {
//
//    // The System File Logger
//    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
//
//    // The Prolog Source File
//    private final String mSource;
//
//    // Initialize The Loader
//    public OLDJPLLoader(final String source) {
//
//        // Give The Thread A Name
//        super("JPLLoader(" + source + ")");
//
//        // Initialize The Source
//        mSource = source;
//    }
//
//    // Execute The JPL Loader
//    @Override
//    public void run() {
//
//        // Call The Query To The Engine
//        final OLDJPLResult result = OLDJPLEngine.query("consult('" + mSource + "').");
//
//        // Print Debug Information
//        mLogger.message("Consulted Source '" + mSource + "' In JPL Engine '" + OLDJPLEngine.string() + "'");
//    }
//}
