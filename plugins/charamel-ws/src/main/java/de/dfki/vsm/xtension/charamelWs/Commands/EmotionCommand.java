package de.dfki.vsm.xtension.charamelWs.Commands;

public class EmotionCommand implements Broadcastable {
    private final String emotionname;
    private final float intensity;

    public EmotionCommand(String name, float intensity) {
        this.emotionname = name;
        this.intensity = intensity;
    }

    public String toJson() {
        return "    {\n" +
                "      \"type\": 'emotion',\n" +
                "      \"name\": '" + emotionname + "',\n" +
                "      \"attack\": 200,\n" +
                "      \"hold\": 1000,\n" +
                "      \"decay\": 200,\n" +
                "      \"value\": " + intensity + "\n" +
                "    }\n";

    }
}
