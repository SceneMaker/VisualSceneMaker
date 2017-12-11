package de.dfki.vsm.editor.util.autocompletation;

import de.dfki.vsm.editor.project.auxiliary.scenescript.ScriptEditorPane;
import org.fife.ui.autocomplete.BasicCompletion;
import org.fife.ui.autocomplete.DefaultCompletionProvider;

import javax.swing.text.JTextComponent;
import javax.swing.text.Segment;
import java.util.ArrayList;
import java.util.HashMap;

public class PluginCompletionProvider extends DefaultCompletionProvider {
    private final HashMap<String, ArrayList> replacements;
    private final ScriptEditorPane editor;
    private String currentCharacter = "";


    public PluginCompletionProvider(HashMap<String, ArrayList> replacements, ScriptEditorPane mEditorPane) {
        seg = new Segment();
        this.completions = new ArrayList();
        this.replacements = replacements;
        this.editor = mEditorPane;

    }

    @Override
    public String getAlreadyEnteredText(JTextComponent jTextComponent) {
        updateCompletions(jTextComponent);
        return super.getAlreadyEnteredText(jTextComponent);
    }

    private void updateCompletions(JTextComponent jTextComponent) {
        if (!isCaretInsideActionBlock()) {
            this.completions = new ArrayList();
        } else {
            addCompletionForCharacter(jTextComponent);
        }
    }

    private void addCompletionForCharacter(JTextComponent jTextComponent) {
        String characterName = findCharacterName(jTextComponent);
        if (!currentCharacter.equals(characterName) || completions.isEmpty()) {
            addCompletionToProvider(characterName);
            currentCharacter = characterName;
        }
    }

    private void addCompletionToProvider(String characterName) {
        if (!characterName.equals("") && replacements.containsKey(characterName)) {
            ArrayList<String> characterReplacements = replacements.get(characterName);
            for (String replacement : characterReplacements) {
                addCompletion(new BasicCompletion(this, replacement));
            }
        }
    }

    private String findCharacterName(JTextComponent jTextComponent) {
        CharacterFinder characterFinder = new CharacterFinder(jTextComponent);
        return characterFinder.getCharacterName();
    }

    private boolean isCaretInsideActionBlock() {
        DocumentCharFinder characterFinder = new DocumentCharFinder(editor);
        characterFinder.updatePositionToCurrentCaret();
        int openBracketsPos = characterFinder.findBackward("[");
        characterFinder.updatePositionToCurrentCaret();
        int closedBracketPos = characterFinder.findBackward("]");
        return (openBracketsPos > closedBracketPos);
    }


}
