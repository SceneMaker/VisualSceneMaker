package de.dfki.vsm.xtension.charamelWs;

import de.dfki.vsm.xtension.charamelWs.Commands.Broadcastable;

public class LookCommand implements Broadcastable {
    private final double xPos;
    private final double yPos;

    public LookCommand(double xPos, double yPos) {
        // Validate parameters
        if (Math.abs(xPos) > 1) {
            this.xPos = 0;
        }
        else {
            this.xPos = xPos;
        }

        if (Math.abs(yPos) > 1) {
            this.yPos = 0;
        }
        else {
            this.yPos = yPos;
        }
    }

    @Override
    public String toJson() {
        return String.format("{\n" +
                "  \"type\": \"lookat\",\n" +
                "  \"x\": %f,\n" +
                "  \"y\": %f\n" +
                "}", xPos, yPos);
    }
}
