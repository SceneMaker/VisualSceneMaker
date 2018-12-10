package de.dfki.vsm.util.jpl;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Gregor Mehlmann
 */
public class JPLUtility {

    final static Pattern sEmptyNILListPattern = Pattern.compile("'\\[\\]'");
    final static Pattern sFillyISOListPattern = Pattern.compile("'\\.'\\((.*)\\)");
    final static Pattern sFillySWIListPattern = Pattern.compile("'\\[\\|\\]'\\((.*)\\)");
    //final static Pattern sKeyValuePairPattern = Pattern.compile(":\\((.+?), (.*)\\)");
    final static Pattern sKeyValuePairPattern = Pattern.compile("':'\\((.*?), (.*)\\)");//[a-zA-Z_]+?

    public final static String convert(final String input) {
        // Create A Matcher For Non-Empty Lists
        final Matcher iso = sFillyISOListPattern.matcher(input);
        final Matcher swi = sFillySWIListPattern.matcher(input);
        final Matcher nil = sEmptyNILListPattern.matcher(input);
        // Check if We Have A Non-Empty ISO or SWI List
        if (iso.matches() || swi.matches()) {
            // Convert A Non-Empty Iso Prolog List
            return "[" + enfold(input) + "]";
        } else if (nil.matches()) {
            // We Have An Empty Iso Or SWI List 
            return "[]";
        } else {
            // We Have An Atomic Or Numeric Value
            // We Can Return Them As They Are Here
            return input;
        }
    }

    public final static String enfold(final String list) {

        // Create A Matcher For Non-Empty Lists
        Matcher iso = sFillyISOListPattern.matcher(list);
        Matcher swi = sFillySWIListPattern.matcher(list);

        // Check if We Have A Non-Empty Iso List
        if (iso.matches() || swi.matches()) {

            // Get The Body Of The Iso Prolog List Now
            final String body = (iso.matches() ? iso.group(1) : swi.group(1));

            //System.out.println("body=" + body);
            // Find The Comma Seperating Head And Body
            int depth = 0,
                    index = 0;

            while (index < body.length()) {

                // The Token At The Current Index
                final char token = body.charAt(index);

                if (token == '(') {
                    depth++;
                } else if (token == ')') {
                    depth--;
                } else if (token == ',') {
                    if (depth == 0) {

                        // Found The Seperating Comma
                        break;
                    }
                }

                // Increment The Counting Index
                index++;
            }

            // Extract The Head And The Tail Of The List
            final String head = body.substring(0, index);
            final String tail = body.substring(index + 2);    // Comma And Whitespace

            //System.out.println("head=" + head);
            //System.out.println("tail=" + tail);
            // Initialize The Final Result Record String
            String result = new String();
            // Create A Matcher For A Key Value Pair
            final Matcher pairm = sKeyValuePairPattern.matcher(head);

            if (pairm.matches()) {
                final String key = pairm.group(1);
                final String val = pairm.group(2);

                // Append The Key Value Pair To The Result
                result += key + ":" + convert(val);

                //System.out.println("key="+key);
                //System.out.println("val="+val);
            } else {
                result += convert(head);
            }

            // Create A Matcher For Empty Iso Lists
            //final Matcher tailm = sFillyISOListPattern.matcher(tail);
            // Create A Matcher For Non-Empty Lists
            iso = sFillyISOListPattern.matcher(tail);
            swi = sFillySWIListPattern.matcher(tail);

            final Matcher nil = sEmptyNILListPattern.matcher(tail);
            if (iso.matches() || swi.matches()) {
                // Append The Tail List To The Result
                result += ", " + enfold(tail);
            } else if (nil.matches()) {
                // Append The Tail List To The Result
                result += "";//, " + enfold(tail);
            } else {
                // Append The Tail List To The Result
                result += "|" + tail;
            }

            // Return the final result
            return result;
        } else {
            // This should never happen
            return null;
        }
    }
}
