package de.dfki.vsm.runtime.dialogacts;

//~--- JDK imports ------------------------------------------------------------

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Sergio SotoSoto
 */
public class DummyDialogAct implements DialogActInterface {
    Map<String, List<String>> mAttributes;
    Map<String, List<String>> mDialogueActs;
    Map<String, String>       mAttributeValueMap1;
    Map<String, String>       mDAUtterance;

    public DummyDialogAct() {
        mAttributes         = new HashMap();
        mDialogueActs       = new HashMap();
        mAttributeValueMap1 = new HashMap();
        mDAUtterance        = new HashMap();
        init();
    }

    private void init() {

        // Create dummy attribute phases and dialogue acts
        List<String> dialogueActs1 = new ArrayList<>();
        List<String> dialogueActs2 = new ArrayList<>();
        List<String> dialogueActs3 = new ArrayList<>();
        List<String> dialogueActs4 = new ArrayList<>();

        dialogueActs1.clear();
        dialogueActs1.add("Greetings");
        dialogueActs1.add("Yes-No-Question");
        mDialogueActs.put("Welcome", dialogueActs1);
        mDAUtterance.put("Greetings", "UtteranceText for Greetings");
        mDAUtterance.put("Yes-No-Question", "UtteranceText for Yes-No-Question");
        mDAUtterance.put("Welcome", "UtteranceText for Welcome");
        dialogueActs2.clear();
        dialogueActs2.add("Statement");
        dialogueActs2.add("Open-Question");
        dialogueActs2.add("Request");
        dialogueActs2.add("Reject");
        mDialogueActs.put("Conversation", dialogueActs2);
        mDAUtterance.put("Statement", "UtteranceText for Statement");
        mDAUtterance.put("Open-Question", "UtteranceText for Open-Question");
        mDAUtterance.put("Request", "UtteranceText for Request");
        mDAUtterance.put("Reject", "UtteranceText for Reject");
        mDAUtterance.put("Conversation", "UtteranceText for Conversation");
        dialogueActs3.clear();
        dialogueActs3.add("Repeat-Phrase");
        mDialogueActs.put("Discussion", dialogueActs3);
        mDAUtterance.put("Repeat-Phrase", "UtteranceText for Repeat-Phrase");
        mDAUtterance.put("Discussion", "UtteranceText for Discussion");
        dialogueActs4.clear();
        dialogueActs4.add("Thanking");
        mDialogueActs.put("Farewell", dialogueActs4);
        mDAUtterance.put("Thanking", "UtteranceText for Thanking");
        mDAUtterance.put("Farewell", "UtteranceText for Farewell");

        // Create dummy attribute values
        List<String> attributeValues1 = new ArrayList<>();
        List<String> attributeValues2 = new ArrayList<>();
        List<String> attributeValues3 = new ArrayList<>();

        attributeValues1.clear();
        attributeValues1.add("Ease");
        attributeValues1.add("Challenge");
        mAttributes.put("Difficulty", attributeValues1);
        mAttributeValueMap1.put("Difficulty", "Ease");
        mAttributeValueMap1.put("Difficulty", "Challenge");
        mDAUtterance.put("Reject", "UtteranceText for Reject");
        attributeValues2.clear();
        attributeValues2.add("Hostile");
        attributeValues2.add("Neutral");
        attributeValues2.add("Friendly");
        mAttributes.put("Attitude", attributeValues2);
        attributeValues3.clear();
        attributeValues3.add("French");
        attributeValues3.add("English");
        mAttributes.put("Language", attributeValues3);
    }

    @Override
    public List<String> getDialogueActPhases() {
        return new ArrayList<>(mDialogueActs.keySet());
    }

    @Override
    public List<String> getDialogueActs(String phase) {
        return new ArrayList(mDialogueActs.get(phase));
    }

    @Override
    public List<String> getNLGAttributes() {
        return new ArrayList<>(mAttributes.keySet());
    }

    @Override
    public List<String> getNLGAttributeValues(String attribute) {
        return new ArrayList<>(mAttributes.get(attribute));
    }

    @Override
    public String getUtterances(String dialogueAct, Map<String, String> map) {
        return mDAUtterance.get(dialogueAct);
    }

    @Override
    public List<String> getBMLCommands() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public String sendFMLCommands(String dialogueAct, String FML, Map<String, String> map) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public List<String> getFMLCommands(String dialogueAct, Map<String, String> map) {
        List<String> fmlCommands = new ArrayList<>();

        fmlCommands.add("fml commands for " + dialogueAct);
        fmlCommands.add("command1");
        fmlCommands.add("command2");

        return fmlCommands;
    }

    @Override
    public List<String> getFMLIntentionClasses() {
        throw new UnsupportedOperationException("Not supported yet.");    // To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public List<String> getFMLIntentions(String intentionClass) {
        throw new UnsupportedOperationException("Not supported yet.");    // To change body of generated methods, choose Tools | Templates.
    }
}
