package de.dfki.vsm.xtension.charamelWs.Commands;

public class GreetCommand extends ActionCommand {
    public GreetCommand(String nr) {
        super("humanoid/interaction/greet/","greet"+nr+".glb");
    }
}
