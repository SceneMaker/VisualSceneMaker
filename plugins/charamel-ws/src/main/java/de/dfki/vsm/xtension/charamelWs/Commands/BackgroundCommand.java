package de.dfki.vsm.xtension.charamelWs.Commands;

public class BackgroundCommand implements Broadcastable {
    private final String url;

    public BackgroundCommand(String url) {
        this.url = url;
    }

    @Override
    public String toJson() {
        return String.format("{\n" +
                "  \"type\": \"background\",\n" +
                "  \"url\": \"%s\"\n" +
                "}", this.url);
    }
}
