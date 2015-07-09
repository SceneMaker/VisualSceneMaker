package de.dfki.vsm.util.tpl;

/**
 * @author Not me
 */
public class TPLTuple<F, S> {

    // The first component
    private F mFirst;
    // The second component
    private S mSecond;

    // Construct a tuple
    public TPLTuple(final F first, final S second) {
        mFirst = first;
        mSecond = second;
    }

    // Get the first component
    public F getFirst() {
        return mFirst;
    }

    // Set the first component
    public void setFirst(final F value) {
        mFirst = value;
    }

    // Get the second component
    public S getSecond() {
        return mSecond;
    }

    // Set the second component
    public void setSecond(final S value) {
        mSecond = value;
    }
}