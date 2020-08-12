package de.dfki.vsm.xtension.charamelWs;

import java.util.UUID;

public class SpeakCommand {
    private final String text;
    private final String voice;
    private final UUID uuid;

    public SpeakCommand(String text, String voice) {
        this.uuid = UUID.randomUUID();
        this.text = text;
        this.voice = voice;
    }

    public String toJsonCommand() {
        return "    {\n" +
                "      \"type\": \"timeline-element\",\n" +
                "      \"subtype\": \"tts\",\n" +
                "      \"uuid\": \"" + uuid + "\",\n" +
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
