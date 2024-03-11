package de.dfki.vsm.xtension.charamelWs.Commands;

public class LuemmelnCommand extends ActionCommand {

    public LuemmelnCommand(int number) {
        super("humanoid/sit/lounge/",
                String.format("sit_lounge_%02d.glb", number));
        if (number > 6 || number < 1) {
            throw new IllegalArgumentException("The character only has five fingers on one hand");
        }
    }
}
