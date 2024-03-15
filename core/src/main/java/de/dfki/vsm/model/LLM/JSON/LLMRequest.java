package de.dfki.vsm.model.LLM.JSON;

public class LLMRequest{
    private String text;
    private String conversation_id;

    public LLMRequest(String text, String conversation_id) {
        this.text = text;
        this.conversation_id = conversation_id;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getConversation_id() {
        return conversation_id;
    }

    public void setConversation_id(String conversation_id) {
        this.conversation_id = conversation_id;
    }
}