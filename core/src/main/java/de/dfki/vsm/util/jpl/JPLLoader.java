package de.dfki.vsm.util.jpl;

/**
 * @author Gregor Mehlmann
 */
public class JPLLoader extends Thread {
    
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
        System.out.println("Consulted '" + mSource + "' in JPL '" + JPLEngine.string() + "'");
    }
}
