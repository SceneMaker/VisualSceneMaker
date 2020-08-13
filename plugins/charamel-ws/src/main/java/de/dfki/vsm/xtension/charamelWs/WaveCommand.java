package de.dfki.vsm.xtension.charamelWs;

import java.util.UUID;

public class WaveCommand {
    private final UUID animation_uuid;
    private final UUID timeline_uuid;

    public WaveCommand() {
        this.animation_uuid = UUID.randomUUID();
        this.timeline_uuid = UUID.randomUUID();
    }

    public String toJsonCommand() {
        return "{\n" +
                "  \"type\": \"animation\",\n" +
                "  \"name\": \"\",\n" +
                "  \"uuid\": \"" + animation_uuid + "\",\n" +
                "  \"timeline\": [\n" +
                "    {\n" +
                "      \"type\": \"timeline-element\",\n" +
                "      \"subtype\": \"motion\",\n" +
                "      \"uuid\": \"" + timeline_uuid + "\",\n" +
                "      \"name\": \"greet01.glb\",\n" +
                "      \"track\": \"motions1_uuid\",\n" +
                "      \"timestamp\": 0,\n" +
                "      \"duration\": 2400,\n" +
                "      \"data\": {\n" +
                "        \"attack\": 500,\n" +
                "        \"decay\": 500,\n" +
                "        \"speed\": 1,\n" +
                "        \"path\": \"humanoid/interaction/greet/greet01.glb\"\n" +
                "      }\n" +
                "    }\n" +
                "  ]\n" +
                "}";

    }
}
