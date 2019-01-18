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

    public static void main(final String[] args) {
        // Initialize JPL at the beginning
        JPL.init();  
        // Create the query and get results
        Query query = new Query("X =[Y|Z]");        
        Map<String, Term>[] solutions = query.allSolutions();
        for (Map<String, Term> solution1 : solutions) {
            System.out.println(Util.toString(solution1));
        }
        // Create the query and get results
        query = new Query("X =[Y,Z]");        
        solutions = query.allSolutions();
        for (Map<String, Term> solution : solutions) {
            System.out.println(Util.toString(solution));
        }
    }
}
