package de.dfki.vsm.util;

import java.util.ArrayList;

public class TextFormat {
    public static String formatConstantStringLiteral(String valueString) {
        ArrayList<Character> charList = new ArrayList<>();
        char[] charArray = valueString.toCharArray();

        for (int i = 0; i < charArray.length; i++) {
            if (charArray[i] == ' ') {
                charList.add(charArray[i]);
            } else {
                if (i == 0) {
                    charList.add('#');
                    charList.add('c');
                    charList.add('#');
                    charList.add(charArray[i]);
                } else {
                    if (charArray[i - 1] == ' ') {
                        charList.add('#');
                        charList.add('c');
                        charList.add('#');
                        charList.add(charArray[i]);
                    } else {
                        charList.add(charArray[i]);
                    }
                }
            }
        }

        char[] formattedCharArray = new char[charList.size()];

        for (int i = 0; i < formattedCharArray.length; i++) {
            formattedCharArray[i] = charList.get(i);
        }

        String formatedString = String.valueOf(formattedCharArray);

        return formatedString;
    }
}
