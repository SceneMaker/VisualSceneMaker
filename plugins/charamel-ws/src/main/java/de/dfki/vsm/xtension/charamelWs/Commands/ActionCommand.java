package de.dfki.vsm.xtension.charamelWs.Commands;

import java.util.UUID;

abstract class ActionCommand implements ICommand {
    private final UUID animation_uuid;
    private final String path;
    private final String filename;

    public ActionCommand(String path, String filename) {
        this.path = path;
        this.filename = filename;
        this.animation_uuid = UUID.randomUUID();
    }

    public String toJsonCommand() {
        return "    {\n" +
                "      \"type\": \"timeline-element\",\n" +
                "      \"subtype\": \"motion\",\n" +
                "      \"uuid\": \"" + animation_uuid + "\",\n" +
                "      \"name\": \"" + filename + "\",\n" +
                "      \"track\": \"motions1_uuid\",\n" +
                "      \"timestamp\": 0,\n" +
                "      \"duration\": 2400,\n" +
                "      \"data\": {\n" +
                "        \"attack\": 500,\n" +
                "        \"decay\": 500,\n" +
                "        \"speed\": 1,\n" +
                "        \"path\": \"" + path + filename + "\"\n" +
                "      }\n" +
                "    }\n";

    }
}
