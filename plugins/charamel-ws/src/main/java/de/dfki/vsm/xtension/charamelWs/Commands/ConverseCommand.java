package de.dfki.vsm.xtension.charamelWs.Commands;

public class ConverseCommand extends ActionCommand {

    public ConverseCommand(Direction direction) {
        super("humanoid/emphasize/converse/", direction == Direction.LEFT ? "converse_left01.glb" : "converse_right01.glb");
    }
}
