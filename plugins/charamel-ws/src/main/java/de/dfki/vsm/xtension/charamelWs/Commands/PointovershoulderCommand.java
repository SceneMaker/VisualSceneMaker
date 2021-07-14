package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointovershoulderCommand extends ActionCommand {

    public PointovershoulderCommand(Direction direction) {
        super("humanoid/Presentation/location", fileName(direction));
    }

    private static String fileName(Direction direction) {
        switch (direction) {
            case LEFT:
                return "In_the_back01.glb";
            case RIGHT:
                return "In_the_back02.glb";
            default:
                throw new IllegalStateException();
        }
    }
}
