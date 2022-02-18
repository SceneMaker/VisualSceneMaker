package de.dfki.vsm.xtension.charamelWs.Commands;

import java.util.Locale;

public class HeadTilt implements Broadcastable {
    private final double xRot;
    private final double yRot;
    private final double zRot;

    public HeadTilt(double xRot, double yRot, double zRot) {
        // Validate parameters
        if (Math.abs(xRot) > 90) {
            this.xRot = 0;
        } else {
            this.xRot = xRot;
        }

        if (Math.abs(yRot) > 90) {
            this.yRot = 0;
        } else {
            this.yRot = yRot;
        }
        if (Math.abs(zRot) > 90) {
            this.zRot = 0;
        } else {
            this.zRot = zRot;
        }
    }

    @Override
    public String toJson() {
        return String.format(Locale.ENGLISH, "{\n" +
                "  \"type\": \"headTilt\",\n" +
                "  \"x\": %f,\n" +
                "  \"y\": %f,\n" +
                "  \"z\": %f\n" +
                "}", xRot, yRot, zRot);
    }
}
