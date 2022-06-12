package de.dfki.vsm.xtension.charamelWs.Commands;

public class EmotionCommand implements Broadcastable {
    private final String emotionname;
    private final float intensity;
    private final int attack;


    public EmotionCommand(String name, float intensity) {
        this.emotionname = name;
        this.intensity = intensity;
        this.attack = 200;
    }

    public EmotionCommand(String name, float intensity, int attack, int hold) {
        this.emotionname = name;
        this.intensity = intensity;
        this.attack = attack;
    }

    public String toJson() {
        return "    {\n" +
                "      \"type\": \"emotion\",\n" +
                "      \"name\": \"" + emotionname + "\",\n" +
                "      \"attack\": " + attack + ",\n" +
                "      \"value\": " + intensity + "\n" +
                "    }\n";
    }
}
