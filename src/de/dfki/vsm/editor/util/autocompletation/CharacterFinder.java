package de.dfki.vsm.editor.util.autocompletation;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

public class CharacterFinder {

    private Document document;
    private DocumentCharFinder charFinder;

    public CharacterFinder(JTextComponent component){
        this.document = component.getDocument();
        this.charFinder = new DocumentCharFinder(component);
    }

    public String getCharacterName(){
        charFinder.updatePositionToCurrentCaret();
        int charPosition = charFinder.getLineStartPosition();
        charFinder.setCurrentPosition(charPosition);
        int colonPosition = charFinder.findForward(":") ;
        int length = colonPosition  - charPosition ;
        return findCharacterName(charPosition, length);
    }

    private String findCharacterName(int charPosition, int length) {
        try {
            return document.getText(charPosition  , length  );
        } catch (BadLocationException e) {
            return "";
        }
    }


}
