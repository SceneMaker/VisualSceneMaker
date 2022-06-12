package de.dfki.vsm.xtension.charamelWs.Commands;

public class LegcrossedCommand extends ActionCommand {

    public LegcrossedCommand(Direction direction) {
        super("humanoid/sit/relax/", direction == Direction.RIGHT ? "sit_relax_middle_01.glb" : "sit_relax_middle_mirrored_01.glb");
    }
}
