package de.dfki.vsm.model.sceneflow.glue.command;

/**
 * @author Gregor Mehlmann
 */
public abstract class Definition extends Command {

    @Override
    public abstract Definition getCopy();
}
