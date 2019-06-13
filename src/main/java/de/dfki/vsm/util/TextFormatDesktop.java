package de.dfki.vsm.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.util.tpl.TPLTuple;

import java.awt.*;
import java.awt.font.TextAttribute;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Gregor Mehlmann
 */
public class TextFormatDesktop extends TextFormat {

    /**
     *
     */
    public static ArrayList<TPLTuple<String, AttributedString>> getPairList(ArrayList<String> formattedStringList) {
        ArrayList<TPLTuple<String, AttributedString>> pairList = new ArrayList<>();

        for (String s : formattedStringList) {
            TPLTuple<String, AttributedString> pair = fillWithAttributes(s);

            pairList.add(pair);
        }

        return pairList;
    }

    /**
     *
     */
    public static TPLTuple<String, AttributedString> fillWithAttributes(String inputString) {
        HashMap<TPLTuple<TextAttribute, Object>, TPLTuple<Integer, Integer>> attributeMap = new HashMap();
        String unformattedString = "";

        while (true) {
            int i = inputString.indexOf('#');

            if (i == -1) {
                unformattedString = unformattedString.concat(inputString);

                break;
            }

            int j = inputString.indexOf(' ', i);
            int k = 0;
            String subString = "";
            String newInputString = "";

            if (j == -1) {
                k = inputString.length() - 1;

                // Get infix
                subString = inputString.substring(i + 3);
            } else {
                k = j;

                // Get infix
                subString = inputString.substring(i + 3, k);
                newInputString = inputString.substring(k);
            }

            // Get format string
            char c = inputString.charAt(i + 1);

            // Get prefix
            String preString = inputString.substring(0, i);

            // Get postfix
            String postString = inputString.substring(i + 3);

            //
            TPLTuple<Integer, Integer> position
                    = new TPLTuple<>(
                    unformattedString.length() + preString.length(),
                    unformattedString.length() + preString.length() + subString.length());

            if (c == 'b') {
                // Highlight with bold weight
                TPLTuple attribute
                        = new TPLTuple(
                                TextAttribute.WEIGHT,
                                TextAttribute.WEIGHT_BOLD);
                attributeMap.put(attribute, position);
                
            } else if (c == 'i') {
                // Highlight with italic posture
                TPLTuple attribute
                        = new TPLTuple(
                                TextAttribute.POSTURE,
                                TextAttribute.POSTURE_OBLIQUE);
                attributeMap.put(attribute, position);
            } else if (c == 'r') {
                // Highlight like reserved word
                TPLTuple attribute1
                        = new TPLTuple(
                                TextAttribute.FOREGROUND, Color.BLUE);
                TPLTuple attribute2
                        = new TPLTuple(
                                TextAttribute.WEIGHT,
                                TextAttribute.WEIGHT_BOLD);

                attributeMap.put(attribute1, position);
                attributeMap.put(attribute2, position);
            } else if (c == 'c') {

                // Constant Literals
                TPLTuple attribute
                        = new TPLTuple(
                                TextAttribute.FOREGROUND,
                                Color.RED.darker());

                attributeMap.put(attribute, position);
            } else if (c == 't') {

                // User-Defined Type Names
                TPLTuple attribute
                        = new TPLTuple(
                                TextAttribute.FOREGROUND,
                                Color.MAGENTA.darker());

                attributeMap.put(attribute, position);
            } else if (c == 'h') {

                // Struct or List Definitions
                TPLTuple attribute
                        = new TPLTuple(
                                TextAttribute.FOREGROUND, Color.BLACK);

                attributeMap.put(attribute, position);
            } else if (c == 'p') {
                // Highlight predefined commands
                TPLTuple attribute1
                        = new TPLTuple(
                                TextAttribute.FOREGROUND,
                                Color.GREEN.darker().darker().darker());
                TPLTuple attribute2
                        = new TPLTuple(
                                TextAttribute.WEIGHT,
                                TextAttribute.WEIGHT_BOLD);

                attributeMap.put(attribute1, position);
                attributeMap.put(attribute2, position);
            }

            unformattedString = unformattedString.concat(preString).concat(subString);
            inputString = newInputString;
        }

        AttributedString attributedString = new AttributedString(unformattedString);

        attributedString.addAttribute(TextAttribute.FOREGROUND, Color.BLACK);
        attributedString.addAttribute(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
        attributedString.addAttribute(TextAttribute.FAMILY, Font.SANS_SERIF);

        if (EditorInstance.getInstance().getSelectedProjectEditor() != null) {
            attributedString.addAttribute(TextAttribute.SIZE, EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sWORKSPACEFONTSIZE);
        } else {
            attributedString.addAttribute(TextAttribute.SIZE, 12);
        }

        // Fill the attributed string with attributes

        for (Map.Entry<TPLTuple<TextAttribute, Object>, TPLTuple<Integer, Integer>> tplTupleTPLTupleEntry : attributeMap.entrySet()) {
            Map.Entry entry = tplTupleTPLTupleEntry;
            TPLTuple<TextAttribute, Object> key = (TPLTuple<TextAttribute, Object>) entry.getKey();
            TPLTuple<Integer, Integer> value = (TPLTuple<Integer, Integer>) entry.getValue();

            attributedString.addAttribute(key.getFirst(), key.getSecond(), value.getFirst(), value.getSecond());
        }

        return new TPLTuple<>(unformattedString, attributedString);
    }


}