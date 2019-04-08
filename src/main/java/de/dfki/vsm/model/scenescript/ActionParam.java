package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.HashMap;

/**
 * @author Gregor Mehlmann
 */
public final class ActionParam extends ActionFeature {

    public ActionParam() {
    }

    public ActionParam(
            final int lower,
            final int upper,
            final String key,
            final String val) {
        // TODO: Get Text and Param
        super(/*Type.VARIABLE, */lower, upper, key, val);
    }

    @Override
    public final String getVal(final HashMap<String, String> args) {
        return args.get(mVal);
    }

    @Override
    public final String getText() {
        return mKey + "=" + "$" + mVal;
    }

    @Override
    public final String getText(final HashMap<String, String> args) {
        return mKey + "=" + args.get(mVal);
    }

    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.print("<ActionParam "
                + "lower=\"" + mLower + "\" "
                + "upper=\"" + mUpper + "\" "
                //+ "typ=\"" + mTyp + "\" "
                + "key=\"" + mKey + "\" "
                + "val=\"" + mVal + "\"/>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        // Parse The Boundary
        mLower = Integer.parseInt(element.getAttribute("lower"));
        mUpper = Integer.parseInt(element.getAttribute("upper"));

        // Parse The Members
        //mTyp = Type.valueOf(element.getAttribute("typ"));
        // Parse The Members
        mKey = element.getAttribute("key");
        mVal = element.getAttribute("val");
    }

    @Override
    public final ActionParam getCopy() {
        return new ActionParam(mLower, mUpper, mKey, mVal);
    }
}
