package de.dfki.vsm.util.jpl;

//~--- JDK imports ------------------------------------------------------------

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Not me
 */
public class JPLUtility {
    final static Pattern sEmptyIsoListPattern = Pattern.compile("\\[\\]");
    final static Pattern sFillyIsoListPattern = Pattern.compile("'\\.'\\((.*)\\)");
    //final static Pattern sKeyValuePairPattern = Pattern.compile(":\\((.+?), (.*)\\)");
    final static Pattern sKeyValuePairPattern = Pattern.compile(":\\(([a-zA-Z]+?), (.*)\\)");
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String convert(final String input) {

        // Create A Matcher For Non-Empty Lists
        final Matcher matcher = sFillyIsoListPattern.matcher(input);

        // Check if We Have A Non-Empty Iso List
        if (matcher.matches()) {

            // Convert A Non-Empty Iso Prolog List
            return "[" + enfold(input) + "]";
        } else {

            // We Either Have An Empty Iso List Or
            // We Have An Atomic Or Numeric Value
            // We Can Return Them As They Are Here
            return input;
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final static String enfold(final String list) {

        // Create A Matcher For Non-Empty Lists
        final Matcher listm = sFillyIsoListPattern.matcher(list);

        // Check if We Have A Non-Empty Iso List
        if (listm.matches()) {

            // Get The Body Of The Iso Prolog List Now
            final String body = listm.group(1);

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

            // Initialize The Final Result Record String
            String result = new String();

            // Create A Matcher For A Key Value Pair
            final Matcher pairm = sKeyValuePairPattern.matcher(head);

            if (pairm.matches()) {
                final String key = pairm.group(1);
                final String val = pairm.group(2);

                // Append The Key Value Pair To The Result
                result += key + ":" + convert(val);
            }

            // Create A Matcher For Empty Iso Lists
            final Matcher tailm = sFillyIsoListPattern.matcher(tail);

            if (tailm.matches()) {

                // Append The Tail List To The Result
                result += ", " + enfold(tail);
            }

            // Return The Final Result
            return result;
        }

        // This Should Never Happen
        return null;
    }
}
