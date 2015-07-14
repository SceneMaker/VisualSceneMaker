package de.dfki.vsm.runtime.dialogacts;

//~--- JDK imports ------------------------------------------------------------

import java.util.List;
import java.util.Map;

/**
 *
 * @author Sergio Soto
 */
public interface DialogActInterface {

    /**
     * Returns list containing all phases
     */
    abstract List<String> getDialogueActPhases();

    /**
     * Returns list containing DA according to given phase, or all if phase is
     * empty
     */
    abstract List<String> getDialogueActs(String phase);

    /**
     * Returns list containing all attributes, e.g. attitude
     */
    abstract List<String> getNLGAttributes();

    /**
     * Returns list containing all values for the given attribute
     */
    abstract List<String> getNLGAttributeValues(String attribute);

    /**
     * Returns string containing all text of all utterances related to the given
     * Map entries of attribute and value pairs.
     */
    abstract String getUtterances(String dialogueAct, Map<String, String> map);

    /**
     * Returns list[string] containing all text of all utterances Map entries of
     * attribute, value pairs, reparse with fml-apml.dtd
     */
    abstract List<String> getFMLCommands(String dialogueAct, Map<String, String> map);

    /**
     * Returns list containing all intention classes for FML
     */
    abstract List<String> getFMLIntentionClasses();

    /**
     * Returns list containing intentions according to given intention class, or
     * all if phase is empty
     */
    abstract List<String> getFMLIntentions(String intentionClass);

    /**
     * Returns list containing all BML commands
     */
    abstract List<String> getBMLCommands();

    /**
     * Save changes
     */
    abstract String sendFMLCommands(String dialogueAct, String FML, Map<String, String> map);
}
