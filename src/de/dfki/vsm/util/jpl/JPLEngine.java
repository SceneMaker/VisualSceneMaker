package de.dfki.vsm.util.jpl;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.util.Map;
import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;

/**
 * @author Gregor Mehlmann
 */
public final class JPLEngine {

    // The System File Logger
    private static LOGDefaultLogger sLogger
            = LOGDefaultLogger.getInstance();
    // The Liveliness Flag
    private static volatile boolean sActive;

    // Load A Prolog Source File
    public static void load(final String source) {
        // Construct A JPL Loader
        final JPLLoader loader = new JPLLoader(source);
        // Start The JPL Loader
        loader.start();
        // Await The JPL Loader
        try {
            // Join The JPL Loader
            loader.join();
        } catch (final InterruptedException exc) {
            // Print Debug Information
            sLogger.failure(exc.toString());
        }
    }

    // Get String Representation
    public static synchronized String string() {
        if (sActive) {
            String[] arg = JPL.getActualInitArgs();
            String argstr = JPL.version_string();

            argstr += ",[";

            for (int i = 0; i < arg.length; i++) {
                argstr += arg[i];

                if (i < arg.length - 1) {
                    argstr += ",";
                }
            }
            argstr += "]";

            return "JPLEngine(" + argstr + ")";
        }
        return null;
    }

    // Initialize The JPL Engine
    public static synchronized void init() {
        if (!sActive) {
            // Initialize JPL Only Once
            JPL.init();
            // Set The Liveliness Flag
            sActive = true;
            // Print Debug Information
            sLogger.message("Initializing JPL Engine '" + string() + "'");
        }
    }

    // Call A Query On The Engine
    public static synchronized JPLResult query(String querystr) {
        // Eventually initialize JPL
        init();
        // Append A Period At End
        if (!querystr.endsWith(".")) {
            querystr += ".";
        }
        // The query of this call
        final Query query = new Query(querystr);
        // The result of this call
        final JPLResult result = new JPLResult(query);
        // Get all query solutions 
        try {
            final Map<String, Term>[] solutions = query.allSolutions();
            // Print Debug Information
            //sLogger.message("Query '" + querystr+ "' has '" + solutions.length + "' solutions");
            for (int i = 0; i < solutions.length; i++) {
                final Map<String, Term> solution = solutions[i];
                // Print the solution bindings
                //System.out.println(Util.toString(solution));
                // Add the solution to result
                result.add(solution);
            }
        } catch (final Exception exc) {
            sLogger.failure(exc.toString());
        } finally {
            // Close The New Query
            query.close();

        }
        return result;
    }
}
