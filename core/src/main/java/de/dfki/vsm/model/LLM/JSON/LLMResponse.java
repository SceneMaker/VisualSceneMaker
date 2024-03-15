package de.dfki.vsm.model.LLM.JSON;

public class LLMResponse{
    private LLMResponseData data;
    private LLMResponseMessage message;

    public LLMResponseData getData() {
        return data;
    }

    public void setData(LLMResponseData data) {
        this.data = data;
    }

    public LLMResponseMessage getMessage() {
        return message;
    }

    public void setMessage(LLMResponseMessage message) {
        this.message = message;
    }

    public String getResponse(){
        return getData().getText();
    }
}
class LLMResponseData {
    private String text;

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }
}

class LLMResponseMessage {
    private String success;

    public String getSuccess() {
        return success;
    }

    public void setSuccess(String success) {
        this.success = success;
    }
}
