package de.dfki.vsm.xtension.charamelWs.Commands;

public class CameraCommand implements Broadcastable {
    private final CameraPos cameraPos;

    public CameraCommand(CameraPos cameraPos) {
        this.cameraPos = cameraPos;
    }

    @Override
    public String toJson() {
        return String.format("{\n" +
                "  type: 'camera',\n" +
                "  name: '%s',\n" +
                "  duration: 3333,\n" +
                "}", cameraPos.toString().toLowerCase());
    }

    public enum CameraPos {
        DEFAULT,
        UPPER,
        FACE
    }
}
