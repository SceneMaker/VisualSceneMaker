package de.dfki.vsm.xtension.charamelWs;

import de.dfki.vsm.xtension.charamelWs.Commands.Broadcastable;

public class LookCommand implements Broadcastable {
    private final double xPos;
    private final double yPos;

    public LookCommand(double xPos, double yPos) {
        // Validate parameters
        if (Math.abs(xPos) > 1 || Math.abs(yPos) > 1) {
            throw new IllegalArgumentException(
                    String.format("Expected x and y to be >= -1 and <= 1\n" +
                            "was: x: %f, y: %f", xPos, yPos)
            );
        }
        this.xPos = xPos;
        this.yPos = yPos;
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
