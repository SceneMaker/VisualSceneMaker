package de.dfki.vsm.editor.util.autocompletation;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;

public class DocumentCharFinder {
    private final Document document;
    private final JTextComponent component;
    private int currentPosition;


    public DocumentCharFinder(JTextComponent jTextComponent) {
        this.document = jTextComponent.getDocument();
        this.component = jTextComponent;
    }

    public void updatePositionToCurrentCaret() {
        this.currentPosition = this.getCurrentCaretPosition();
    }

    public void setCurrentPosition(int newPosition) {
        currentPosition = newPosition;
    }

    public int findForward(String charToFind) {
        try {
            return tryToFindCharForward(charToFind);
        } catch (BadLocationException e) {
            return -1;
        }
    }

    private int tryToFindCharBackward(String charToFind) throws BadLocationException {
        int charPosition = this.currentPosition;
        String currentChar = "";
        while (!currentChar.equals(charToFind)) {
            currentChar = document.getText(charPosition, 1);
            charPosition--;
        }
        return charPosition + 1;
    }

    public int findBackward(String charToFind) {
        try {
            return tryToFindCharBackward(charToFind);
        } catch (BadLocationException e) {
            return -1;
        }

    }

    private int tryToFindCharForward(String charToFind) throws BadLocationException {
        int charPosition = this.currentPosition;
        String currentChar = "";
        while (!currentChar.equals(charToFind)) {
            currentChar = document.getText(charPosition, 1);
            charPosition++;
        }
        return charPosition - 1;
    }

    public int getLineStartPosition() {
        int dot = getCurrentCaretPosition();
        Element root = document.getDefaultRootElement();
        int index = root.getElementIndex(dot);
        Element elem = root.getElement(index);
        return elem.getStartOffset();
    }

    public int getCurrentCaretPosition() {
        return component.getCaretPosition();

    }
}
