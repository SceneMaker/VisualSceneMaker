package de.dfki.vsm.xtension.charamelWs.Commands;

public class SpeakingCommand extends ActionCommand {
    public SpeakingCommand(String nr) {
        super("humanoid/interaction/speak/","speak"+nr+".glb");
    }
}
