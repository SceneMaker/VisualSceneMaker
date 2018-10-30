package de.dfki.vsm.xtesting.jpl;

import de.dfki.vsm.util.jpl.JPLResult;
import de.dfki.vsm.util.jpl.JPLEngine;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

/**
 * @author Gregor Mehlmann
 */
public class JPLTestSuite {

    public static void main(final String args[]) throws IOException {
        if (args.length == 2) {
            final File in = new File(args[0]);
            final File out = new File(args[1]);

            // Eventually create output file
            if (!out.exists()) {
                out.createNewFile();
            }
            //
            final BufferedWriter writer = new BufferedWriter(new FileWriter(out));
            //
            if (in.exists()) {
                try {
                    System.out.println("JPL TestSuite File: " + in.getAbsolutePath());
                    // Scann the input in
                    final Scanner sScanner = new Scanner(in, "UTF-8");
                    int counter = 0;
                    while (sScanner.hasNextLine()) {
                        final String line = sScanner.nextLine();
                        //System.err.println(counter++ + ": " + line);
                        
                        if (!line.startsWith("%")) {
                            System.out.println("Line: " + line);
                            final JPLResult result = JPLEngine.query(line);
                             System.out.println("Result: " + result.toString());
                            writer.write("\n?- " + line + "\n    " + result.toString());
                            
                            final JPLResult cleaned = result.clean();
                            
                            writer.write("\n    " + cleaned.toString());
                            
                            writer.newLine();
                            writer.flush();
                            //System.out.println(line + ":\n" /*+ result.toString() + "\n" */ + result.getText());
                        }
                    }
                } catch (final Exception exc) {
                    exc.printStackTrace();
                }
            }

            writer.close();
        }
    }
}
