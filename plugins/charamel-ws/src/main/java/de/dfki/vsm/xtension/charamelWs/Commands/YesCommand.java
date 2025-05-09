package de.dfki.vsm.xtension.charamelWs.Commands;

public class YesCommand extends ActionCommand {
    public YesCommand(String nr) {
        super("humanoid/interaction/yes/","yes"+nr+".glb");
    }
}
