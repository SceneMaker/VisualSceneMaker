package de.dfki.vsm.xtension.charamelWs.Commands;

public class BackgroundCommand implements ICommand {
    private final String url;

    public BackgroundCommand(String url) {
        this.url = url;
    }

    @Override
    public String toJsonCommand() {
        return String.format("{\n" +
                "  type: 'background',\n" +
                "  url:\n" +
                "    '%s',\n" +
                "}", this.url);
    }
}
