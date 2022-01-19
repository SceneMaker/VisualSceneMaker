package de.dfki.vsm.xtension.charamelWs.Commands;

public class CountLeftCommand extends ActionCommand {

    public CountLeftCommand(int number) {
        super("humanoid/Presentation/numbers/",
                String.format("Number_handl_1-%02d.glb", number));
        if (number > 5 || number < 1) {
            throw new IllegalArgumentException("The character only has five fingers on one hand");
        }
    }
}
