package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointOpenPalmCommand extends ActionCommand {

    public PointOpenPalmCommand(Direction direction) {
        super("humanoid/presentation/location/", fileName(direction));
    }

    private static String fileName(Direction direction) {
        switch (direction) {
            case RIGHT:
                return "on_the_right02.glb";
            case LEFT:
                return "on_the_left02.glb";
            default:
                throw new IllegalStateException();
        }
    }
}
