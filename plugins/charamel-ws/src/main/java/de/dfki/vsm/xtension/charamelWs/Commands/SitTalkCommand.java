package de.dfki.vsm.xtension.charamelWs.Commands;

public class SitTalkCommand extends ActionCommand {

    public SitTalkCommand(int talk_step) {
        super("humanoid/sit/talk/", String.format("sit_talk_%02d.glb", talk_step));
    }
}
