package de.dfki.vsm.runtime.error;

/**
 * An exception thrown if a run time error was detected
 *
 * @author Gregor Mehlmann
 */
public class RunTimeException extends InterpreterException {

    public RunTimeException(Object obj, String msg) {
        super(obj, msg);
    }
}
