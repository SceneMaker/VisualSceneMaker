package de.dfki.vsm.xtension.charamelWs.Commands;

import java.util.UUID;

public class TimeLine implements Broadcastable {
    private static UUID UUID;
    private final ICommand command;

    public TimeLine(ICommand command) {
        this.command = command;
        UUID = java.util.UUID.randomUUID();
    }

    @Override
    public String toJson() {
        return ("{\n" +
                "  \"type\": \"animation\",\n" +
                "  \"name\": \"\",\n" +
                "  \"uuid\": \"" + UUID + "\",\n" +
                "  \"timeline\": [\n" +
                command.toJsonCommand() +
                "  ]\n" +
                "}");
    }
}
