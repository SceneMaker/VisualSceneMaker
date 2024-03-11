package de.dfki.vsm.xtension.charamelWs.Commands;

import java.util.UUID;

public class SpeakCommand implements ICommand {
    private final String text;
    private final String voice;
    private final UUID uuid;
    private final String actorId;

    public SpeakCommand(String text, String voice, String actorId) {
        this.actorId = actorId;
        this.uuid = UUID.randomUUID();
        this.text = text;
        this.voice = voice;
    }

    public String toJsonCommand() {
        return "    {\n" +
                "      \"type\": \"timeline-element\",\n" +
                "      \"subtype\": \"tts\",\n" +
                "      \"uuid\": \"" + uuid + "\",\n" +
                "      \"name\": \"" + actorId + "\",\n" +
                "      \"track\": \"uuid_tts\",\n" +
                "      \"timestamp\": 200,\n" +
                "      \"duration\": 5000,\n" +
                "      \"data\": {\n" +
                "        \"text\": \"" + text + "\",\n" +
                "        \"voice\": \"" + voice + "\"\n" +
                "      }\n" +
                "    }\n";
    }
}
