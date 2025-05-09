package de.dfki.vsm.xtension.charamelWs.Commands;

public class PointLCommand extends ActionCommand {
    public PointLCommand(String nr) {
        super("humanoid/presentation/point/","point_handl_"+nr+".glb");
    }
}
