package de.dfki.vsm.editor.util.autocompletation;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;

public class CharacterFinder {

    private Document document;

    public String getCharacterName(JTextComponent jTextComponent){
        int charPosition = getCurrentCharacterPosition(jTextComponent);
        return findCharacterName(charPosition);
    }

    private String findCharacterName(int charPosition) {

        try {
            int colonPosition = goBackwardsToFindChr(charPosition, ":") ;
            int length = colonPosition  - charPosition ;
            return document.getText(charPosition  , length  );
        } catch (BadLocationException ble) {
            ble.printStackTrace();

        }
        return "";
    }

    private int goBackwardsToFindChr(int charPosition, String charToFind) throws BadLocationException {
        String currentChar = "";
        while (!currentChar.equals(charToFind)){
            currentChar = document.getText(charPosition, 1);
            charPosition++;
        }
        return charPosition - 1;
    }

    private  int getCurrentCharacterPosition(JTextComponent jTextComponent) {
        document = jTextComponent.getDocument();
        int dot = jTextComponent.getCaretPosition();
        Element root = document.getDefaultRootElement();
        int index = root.getElementIndex(dot);
        Element elem = root.getElement(index);
        return elem.getStartOffset();
    }
}
