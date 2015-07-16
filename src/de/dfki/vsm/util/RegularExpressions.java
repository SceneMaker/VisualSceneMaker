package de.dfki.vsm.util;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Gregor Mehlmann
 */
public final class RegularExpressions {

    public static final String WHITE = "[\\ \\t\\b\\r\\f]";
    public static final String ALPHA = "[a-zA-Z\u00e4\u00c4\u00f6\u00d6\u00fc\u00dc\u00df\u0040\u00b5]";
    public static final String DIGIT = "[0-9]";

    public static ArrayList<String> getMatches(String input, String regexp, int num) {
        Pattern pattern = Pattern.compile(regexp);
        Matcher matcher = pattern.matcher(input);
        ArrayList<String> match = new ArrayList<String>();

        while (matcher.find()) {
            for (int i = 0; i < num; i++) {
                match.add(matcher.group(i));
            }
        }

        return match;
    }
}
