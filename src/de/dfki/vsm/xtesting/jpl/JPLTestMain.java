package de.dfki.vsm.xtesting.jpl;
import java.util.Map;
import org.jpl7.JPL;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Util;

/**
 * @author Gregor Mehlmann
 */
public class JPLTestMain {

    public static void main(final String args[]) {
        // Initialize JPL at the beginning
        JPL.init();  
        // Create the query and get results
        Query query = new Query("X =[Y|Z]");        
        Map<String, Term>[] solutions = query.allSolutions();
        for (int i = 0; i < solutions.length; i++) {
            System.out.println(Util.toString(solutions[i]));
        }
        // Create the query and get results
        query = new Query("X =[Y,Z]");        
        solutions = query.allSolutions();
        for (int i = 0; i < solutions.length; i++) {
            System.out.println(Util.toString(solutions[i]));
        }
    }
}
