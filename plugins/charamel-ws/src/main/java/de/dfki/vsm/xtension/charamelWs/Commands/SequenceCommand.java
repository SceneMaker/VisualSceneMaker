package de.dfki.vsm.xtension.charamelWs.Commands;

public class SequenceCommand implements Broadcastable{


    private String sequenceName;

    public SequenceCommand(String sequenceName) {
        this.sequenceName = sequenceName;
    }

    @Override
    public String toJson() {
        return " {\n" +
                "      \"type\": \"sequence\",\n" +
                "      \"name\": \"" + sequenceName + "\"\n" +
                "    }\n";
    }

}
