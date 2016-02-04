package de.dfki.vsm.util.cpy;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * @author Not me
 */
public abstract class CopyTool {

    // The Logger Instance
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // Return A Deep Copy 
    public final static Copyable copy(final Copyable obj) {
        try {
            // Write Out The Object
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final ObjectOutputStream oos = new ObjectOutputStream(bos);
            // Write Out The Object
            oos.writeObject(obj);
            // Read In The Object
            final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
            final ObjectInputStream ois = new ObjectInputStream(bis);
            // Read In The Object            
            final Copyable copy = (Copyable) ois.readObject();
            // Return The Copy Now
            return copy;
        } catch (Exception exc) {
            // Print Some Information
            sLogger.failure(exc.toString());
            // Return The Null Object
            return null;
        }
    }
}
