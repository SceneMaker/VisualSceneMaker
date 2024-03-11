package de.dfki.vsm.model.LLM;

import de.dfki.vsm.util.log.LOGDefaultLogger;

public class LLMController {
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    public LLMController() {
    }

    //this function is called when the play button in pressed in the LLM registry panel
    //the input text is from is the content of the textfield
    public void execute(String text){
        mLogger.warning("LLMController was called with: " + text );
    }
}
