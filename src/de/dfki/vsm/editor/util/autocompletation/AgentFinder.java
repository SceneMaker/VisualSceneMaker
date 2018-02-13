package de.dfki.vsm.editor.util.autocompletation;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

class AgentFinder {

    private Document document;
    private DocumentCharFinder charFinder;

    AgentFinder(JTextComponent component) {
        this.document = component.getDocument();
        this.charFinder = new DocumentCharFinder(component);
    }

    String getCharacterName() {
        charFinder.updatePositionToCurrentCaret();
        int lineStartPosition = charFinder.getLineStartPosition();
        charFinder.setCurrentPosition(lineStartPosition);
        int colonPosition = charFinder.findForward(":");
        int length = colonPosition - lineStartPosition;
        return findCharacterName(lineStartPosition, length);
    }

    private String findCharacterName(int charPosition, int length) {
        try {
            return document.getText(charPosition, length);
        } catch (BadLocationException e) {
            return "";
        }
    }


}
