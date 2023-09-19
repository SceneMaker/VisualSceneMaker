package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public class ActionFeature extends ScriptEntity {

    /*
    public enum Type {

        STRING,
        BOOLEAN,
        INTEGER,
        FLOATING,
        IDENTIFIER,
        VARIABLE
    }

    // The Type Of The Feature
    protected Type mTyp;
     */
    // The Key Of The Member
    protected String mKey;

    // The Value Of The Member
    protected String mVal;

    public ActionFeature() {
    }

    public ActionFeature(
            //final Type typ,
            final int lower,
            final int upper,
            final String key,
            final String val) {
        super(lower, upper);

        // Initialize The Members
        //mTyp = typ;
        mKey = key;
        mVal = val;
    }

    /*
    public final Type getTyp() {
        return mTyp;
    }

    public final void setTyp(final Type typ) {
        mTyp = typ;
    }
     */
    public final String getKey() {
        return mKey;
    }

    public final void setKey(final String key) {
        mKey = key;
    }

    /** Returns the value of the action feature.
     * Beware, this version retains the single-quotes if they were used in the editor.
     *
     * @return The value of the action feature.
     */
    public final String         getVal() {
        return mVal;
    }

    /** Alternative version of @getVal which removes the single-quotes.
     *
      * @return The value after removing (if present) the leading and trailing single-quotes.
     */
    public final String getValNoQuotes() {
        if (mVal.startsWith("'")
                && mVal.endsWith("'")) {
            String out = mVal.substring(1,mVal.length()-1) ;
            return out ;
        } else {
            return mVal ;
        }
    }


    public final void setVal(final String val) {
        mVal = val;
    }

    @Override
    public String getText() {
        return mKey + "=" + mVal;
    }

    public String getVal(final HashMap<String, String> args) {
        return mVal;
    }

    @Override
    public String getText(final HashMap<String, String> args) {
        return mKey + "=" + mVal;
    }

    @Override
    public void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.print("<ActionFeature "
                + "lower=\"" + mLower + "\" "
                + "upper=\"" + mUpper + "\" "
                //+ "typ=\"" + mTyp + "\" "
                + "key=\"" + mKey + "\" "
                + "val=\"" + mVal + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mLower = Integer.parseInt(element.getAttribute("lower"));
        mUpper = Integer.parseInt(element.getAttribute("upper"));
        //mTyp = Type.valueOf(element.getAttribute("typ"));
        mKey = element.getAttribute("key");
        mVal = element.getAttribute("val");
    }

    @Override
    public ActionFeature getCopy() {
        return new ActionFeature(/*mTyp,*/mLower, mUpper, mKey, mVal);
    }
}
