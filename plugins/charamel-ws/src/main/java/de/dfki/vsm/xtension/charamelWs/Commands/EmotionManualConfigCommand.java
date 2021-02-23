package de.dfki.vsm.xtension.charamelWs.Commands;

public class EmotionManualConfigCommand implements Broadcastable {
    private final String emotionname;
    private final float intensity;
    private final int attack;


    public EmotionManualConfigCommand(String name, float intensity) {
        this(name, intensity, 200);
    }

    public EmotionManualConfigCommand(String name, float intensity, int attack) {
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
