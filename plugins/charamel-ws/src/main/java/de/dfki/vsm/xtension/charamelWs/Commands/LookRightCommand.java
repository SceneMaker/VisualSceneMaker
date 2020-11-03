package de.dfki.vsm.xtension.charamelWs.Commands;

public class LookRightCommand extends ActionCommand {

    public LookRightCommand(int stepping) {
        super("humanoid/Presentation/look/", GetGLBForStepping(stepping));
    }

    private static String GetGLBForStepping(int stepping) {
        switch (stepping) {
            case 20: {
                return "Lookto_right20_01.glb";
            }
            case 40: {
                return "Lookto_right40_01.glb";
            }
            case 60: {
                return "Lookto_right60_01.glb";
            }
            default: {
                throw new IllegalArgumentException(String.format("Stepping %s is invalid", stepping));
            }
        }
    }
}
