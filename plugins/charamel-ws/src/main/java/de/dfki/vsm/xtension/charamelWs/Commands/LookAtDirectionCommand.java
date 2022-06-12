package de.dfki.vsm.xtension.charamelWs.Commands;

public class LookAtDirectionCommand extends ActionCommand {

    public LookAtDirectionCommand(int clockPosX, int clockPosY) {
        super("humanoid/presentation/look/",
                String.format("lookat_%02d_%02d.glb", clockPosX, clockPosY));
        if (clockPosX > 12 || clockPosY > 12 || clockPosX < 1 || clockPosY < 1) {
            throw new IllegalArgumentException("only 1 - 12 are allowed");
        }
    }
}
