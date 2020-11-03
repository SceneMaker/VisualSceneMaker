package de.dfki.vsm.xtension.charamelWs.Commands;

public class ArmbewegungenCommand extends ActionCommand {
    public ArmbewegungenCommand(Direction direction) {
        super("humanoid/interaction/Sit/present/", fileName(direction));
    }

    private static String fileName(Direction direction) {
        switch (direction) {
            case LEFT:
                return "Sit_present_handl_2/3";
            case RIGHT:
                return "sit_present_handr_10/9";
            default:
                throw new IllegalStateException();
        }
    }
}
