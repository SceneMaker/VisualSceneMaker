package de.dfki.vsm.util.tpl;

/**
 * @author Not me
 */
public class TPLTriple<F, S, T> {

    // Get the first component
    private F mFirst;
    // Get the seond component
    private S mSecond;
    // Get the third component
    private T mThird;

    // Construct a triple
    public TPLTriple(final F first, final S second, final T third) {
        mFirst = first;
        mSecond = second;
        mThird = third;
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

    // Get the thrid component
    public T getThird() {
        return mThird;
    }

    // Set the third component
    public void setThird(final T value) {
        mThird = value;
    }
}