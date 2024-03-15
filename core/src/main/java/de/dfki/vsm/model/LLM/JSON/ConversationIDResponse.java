package de.dfki.vsm.model.LLM.JSON;


public class ConversationIDResponse {
    private ConversationIDData data;
    private ConversationIDMessage message;

    public ConversationIDData getData() {
        return data;
    }

    public void setData(ConversationIDData data) {
        this.data = data;
    }

    public ConversationIDMessage getMessage() {
        return message;
    }
    public int getConversationID(){
        return getData().getConversation_id();
    }

    public void setMessage(ConversationIDMessage message) {
        this.message = message;
    }
}

class ConversationIDData {
    private int conversation_id;

    public int getConversation_id() {
        return conversation_id;
    }

    public void setConversation_id(int conversation_id) {
        this.conversation_id = conversation_id;
    }
}

class ConversationIDMessage {
    private String success;

    public String getSuccess() {
        return success;
    }

    public void setSuccess(String success) {
        this.success = success;
    }
}
