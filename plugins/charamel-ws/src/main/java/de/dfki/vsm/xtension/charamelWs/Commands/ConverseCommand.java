package de.dfki.vsm.xtension.charamelWs.Commands;

public class ConverseCommand extends ActionCommand {

    public ConverseCommand(Direction direction) {
        super("humanoid/Emphasize/converse/", direction == Direction.LEFT ? "Converse_left01.glb" : "converse_right01.glb");
    }
}
