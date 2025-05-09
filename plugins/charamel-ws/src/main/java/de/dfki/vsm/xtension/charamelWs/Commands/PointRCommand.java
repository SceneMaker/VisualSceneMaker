package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointRCommand extends ActionCommand {
    public PointRCommand(String nr) {
        super("humanoid/presentation/point/","point_handR_"+nr+".glb");
    }
}
