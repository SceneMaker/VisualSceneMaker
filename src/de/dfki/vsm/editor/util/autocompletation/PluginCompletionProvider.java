package de.dfki.vsm.editor.util.autocompletation;

import de.dfki.vsm.editor.project.auxiliary.scenescript.ScriptEditorPane;
import org.fife.ui.autocomplete.*;

import javax.swing.text.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class PluginCompletionProvider extends DefaultCompletionProvider {
    private final HashMap<String, ArrayList> replacements;
    private final ScriptEditorPane editor;
    protected Segment seg;
    private String lastCompletionsAtText;
    private List<Completion> lastParameterizedCompletionsAt;
    private String currentCharacter = "";



    public PluginCompletionProvider(HashMap<String, ArrayList> replacements, ScriptEditorPane mEditorPane) {
        seg = new Segment();
        this.completions = new ArrayList();
        this.replacements = replacements;
        this.editor = mEditorPane;

    }

    @Override
    public String getAlreadyEnteredText(JTextComponent jTextComponent) {
        if(!isValid()){
            this.completions = new ArrayList();
        }else{

            addCompletionForCharacter(jTextComponent);
        }
        return super.getAlreadyEnteredText(jTextComponent);
    }

    private void addCompletionForCharacter(JTextComponent jTextComponent) {
        String characterName = findCharacterName(jTextComponent);
        if(!currentCharacter.equals(characterName) || completions.isEmpty()){
            addCompletionToProvider(characterName);
            currentCharacter = characterName;
        }
    }

    private void addCompletionToProvider(String characterName) {
        if(!characterName.equals("") && replacements.containsKey(characterName)){
            ArrayList<String> characterReplacements = replacements.get(characterName);
            for (String replacement: characterReplacements) {
                addCompletion(new BasicCompletion(this, replacement));
            }
        }
    }

    private String findCharacterName(JTextComponent jTextComponent) {
        CharacterFinder characterFinder = new CharacterFinder();
        return characterFinder.getCharacterName(jTextComponent);
    }

    private boolean isValid(){
        CharacterFinder characterFinder = new CharacterFinder();
        Document document = editor.getDocument();
        int caretPosition = editor.getCaretPosition();
        characterFinder.setDocument(document);
        int lineStartPos = characterFinder.getLineStartPosition(caretPosition);
        int currentPos = caretPosition;
        boolean isValid = false;
        String currentChar = "";
        while (currentPos > lineStartPos  && !isValid){
            try {
                currentChar = document.getText(currentPos, 1);
                if(currentChar.equals("[")){
                    isValid = true;
                }else if (currentChar.equals("]")){
                    isValid = false;
                    break;
                }

                currentPos--;
            } catch (BadLocationException e) {
                return false;
            }

        }
        return isValid;
    }




}
