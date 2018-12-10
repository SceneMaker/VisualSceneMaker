package de.dfki.vsm.editor.util.autocompletation;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

class AgentFinder {

    private final Document document;
    private final DocumentCharFinder charFinder;

    AgentFinder(JTextComponent component) {
        this.document = component.getDocument();
        this.charFinder = new DocumentCharFinder(component);
    }

    String getAgentName() {
        int lineStartPosition = positionCharFinderAtBeginningOfTheLine();
        int colonPosition = charFinder.findForward(":");
        int length = colonPosition - lineStartPosition;
        return findAgentName(lineStartPosition, length);
    }

    private int positionCharFinderAtBeginningOfTheLine() {
        charFinder.updatePositionToCurrentCaret();
        int lineStartPosition = charFinder.getLineStartPosition();
        charFinder.setCurrentPosition(lineStartPosition);
        return lineStartPosition;
    }

    private String findAgentName(int charPosition, int length) {
        try {
            return document.getText(charPosition, length);
        } catch (BadLocationException e) {
            return "";
        }
    }


}
