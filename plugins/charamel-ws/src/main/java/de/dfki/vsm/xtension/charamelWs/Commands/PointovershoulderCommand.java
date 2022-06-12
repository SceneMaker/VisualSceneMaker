package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointovershoulderCommand extends ActionCommand {

    public PointovershoulderCommand(Direction direction) {
        super("humanoid/presentation/location", fileName(direction));
    }

    private static String fileName(Direction direction) {
        switch (direction) {
            case LEFT:
                return "in_the_back01.glb";
            case RIGHT:
                return "in_the_back02.glb";
            default:
                throw new IllegalStateException();
        }
    }
}
