package de.dfki.vsm.xtension.charamelWs;

public class SpeakCommand {
    private final String text;
    private final String voice;

    public SpeakCommand(String text, String voice) {
        this.text = text;
        this.voice = voice;
    }

    public String toJsonCommand() {
        return "    {\n" +
                "      \"type\": \"timeline-element\",\n" +
                "      \"subtype\": \"tts\",\n" +
                "      \"uuid\": \"606fee1b-ad84-406d-9667-bd5822291166\",\n" +
                "      \"name\": \"hello\",\n" +
                "      \"track\": \"uuid_tts\",\n" +
                "      \"timestamp\": 200,\n" +
                "      \"duration\": 5000,\n" +
                "      \"data\": {\n" +
                "        \"text\": \"" + text + "\",\n" +
                "        \"voice\": \"" + voice + "\"\n" +
                "      }\n" +
                "    },\n";
    }
}
