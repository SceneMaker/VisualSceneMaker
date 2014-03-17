package de.dfki.vsm.util.cpy;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * @author Gregor Mehlmann
 */
public class CopyTool {

    // The Logger Instance
    private final static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // Make A Deep Copy Of An Object Which Has To Be Serializable.
    public final static Serializable copy(final Serializable obj) {
        try {
            // Write Out The Object
            final ByteArrayOutputStream bos = new ByteArrayOutputStream();
            final ObjectOutputStream oos = new ObjectOutputStream(bos);
            oos.writeObject(obj);
            // Read In The Object
            final ByteArrayInputStream bis = new ByteArrayInputStream(bos.toByteArray());
            final ObjectInputStream ois = new ObjectInputStream(bis);
            final Serializable copy = (Serializable) ois.readObject();
            return copy;
        } catch (Exception exc) {
            // Print Some Information
            sLogger.failure(exc.toString());
            System.out.println("ex:" + exc);
            // Return The Null Object
            return null;
        }
    }
}
