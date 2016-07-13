package de.dfki.vsm.xtension.unity.commands;

/**
 * Base class for all commands send to the Unity3D plugin.
 * @author J.-L. Himbert <s9jehimb@stud.uni-saarland.de>
 */
public abstract class Command {
    /**
     * Stores the command code.
     */
    private final byte _code;

    /**
     * Stores the id of the command.
     */
    private final int _id;

    /**
     * Creates a new instance of the {@code Command} class and assigns the passed
     * values to the pre-defined properties.
     * 
     * @param code The identifying code of the command.
     * @param id   The id of the new command instance.
     */
    protected Command(byte code, int id) {
        _code = code;
        _id = id;
    }

    /**
     * Gets the command code that is used to identify the type of the command.
     * IMPORTANT: The byte returned *must* be interpreted as unsigned byte.
     * 
     * @return The identifying code of the command.
     */
    public byte getCode() {
        return  _code;
    }
    
    /**
     * Gets the id of the current command instance. Each instance can be identified
     * by this id.
     * 
     * @return The id of the current command instance.
     */
    public final int getCommandId() {
        return _id;
    }

    /**
     * Gets the command's data as a byte array. Typically, this are some additional 
     * parameters required when the command is executed.
     * 
     * @return The command's data as byte array.
     */
    public abstract byte[] GetData();
}