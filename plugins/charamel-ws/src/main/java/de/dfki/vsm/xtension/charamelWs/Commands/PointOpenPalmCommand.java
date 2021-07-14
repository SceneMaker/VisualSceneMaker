package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointOpenPalmCommand extends ActionCommand {

    public PointOpenPalmCommand(Direction direction) {
        super("humanoid/Presentation/location/", fileName(direction));
    }

    private static String fileName(Direction direction) {
        switch (direction) {
            case RIGHT:
                return "On_the_right02.glb";
            case LEFT:
                return "On_the_left02.glb";
            default:
                throw new IllegalStateException();
        }
    }
}
