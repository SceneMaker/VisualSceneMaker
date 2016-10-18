package de.dfki.vsm.model.gesticon;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.io.ByteArrayOutputStream;

/**
 * @author Gregor Mehlmann
 */
public class GesticonGesture implements ModelObject {
    private long    mPreparationEnd   = -1;
    private long    mRetractionStart  = -1;
    private long    mStrokePhaseStart = -1;
    private long    mStrokePhaseEnd   = -1;
    private String  mCharacter;
    private String  mAnimName;
    private String  mAnimPath;
    private String  mCategory;
    private boolean mBlendable;
    private long    mDuration;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public GesticonGesture() {}

    public GesticonGesture(final String character, final String animname, final String animpath, final String category,
                           final boolean blendable, final long duration, final long preparationEnd,
                           final long retractStart, final long strokeStart, final long strokeEnd) {
        mCharacter        = character;
        mAnimPath         = animname;
        mAnimName         = animpath;
        mCategory         = category;
        mBlendable        = blendable;
        mDuration         = duration;
        mPreparationEnd   = preparationEnd;
        mRetractionStart  = retractStart;
        mStrokePhaseStart = strokeStart;
        mStrokePhaseEnd   = strokeEnd;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public String getCharacter() {
        return mCharacter;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public String getAnimPath() {
        return mAnimPath;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public String getAnimName() {
        return mAnimName;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public long getDuration() {
        return mDuration;
    }

    public long getPreparationEnd() {
        return mPreparationEnd;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public long getRetractStart() {
        return mRetractionStart;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public long getStrokeStart() {
        return mStrokePhaseStart;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public long getStrokeEnd() {
        return mStrokePhaseEnd;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public String getCategory() {
        return mCategory;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public boolean isBlendable() {
        return mBlendable;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String toScript() {
        return "[ gesture " + "character=\"" + mCharacter + "\" " + "animname=\"" + mAnimName + "\" " + "animpath=\""
               + mAnimPath + "\" " + "blendable=\"" + mBlendable + "\" " + "duration=\"" + mDuration + "\" "
               + "preparation-end=\"" + mPreparationEnd + "\" " + "retraction-start=\"" + mRetractionStart + "\" "
               + "stroke-phase-start=\"" + mStrokePhaseStart + "\" " + "stroke-phase-end=\"" + mStrokePhaseEnd + "\" "
               + "]";
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.print("<Gesture " + "character=\"" + mCharacter + "\" " + "animname=\"" + mAnimName + "\" "
                     + "animpath=\"" + mAnimPath + "\" " + "category=\"" + mCategory + "\" " + "blendable=\""
                     + mBlendable + "\" " + "duration=\"" + mDuration + "\" " + "preparation-end=\"" + mPreparationEnd
                     + "\" " + "retraction-start=\"" + mRetractionStart + "\" " + "stroke-phase-start=\""
                     + mStrokePhaseStart + "\" " + "stroke-phase-end=\"" + mStrokePhaseEnd + "\" " + "/>");
        stream.flush();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public void parseXML(Element element) throws XMLParseError {
        mCharacter        = element.getAttribute("character");
        mAnimName         = element.getAttribute("animname");
        mAnimPath         = element.getAttribute("animpath");
        mCategory         = element.getAttribute("category");
        mBlendable        = Boolean.valueOf(element.getAttribute("blendable"));
        mDuration         = Long.valueOf(element.getAttribute("duration"));
        mPreparationEnd   = Long.valueOf(element.getAttribute("preparation-end"));
        mRetractionStart  = Long.valueOf(element.getAttribute("retraction-start"));
        mStrokePhaseStart = Long.valueOf(element.getAttribute("stroke-phase-start"));
        mStrokePhaseEnd   = Long.valueOf(element.getAttribute("stroke-phase-end"));
    }

//  //////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {

        // Create A Byte Array Stream
        final ByteArrayOutputStream buffer = new ByteArrayOutputStream();

        // Initialize The Indent Writer
        final IOSIndentWriter stream = new IOSIndentWriter(buffer);

        try {

            // Write Object
            writeXML(stream);
        } catch (XMLWriteError exc) {

            // mLogger.failure(exc.toString());
        }

        // Cleanup Stream and Writer
        stream.flush();
        stream.close();

        // Return String Representation
        try {
            return buffer.toString("UTF-8");
        } catch (Exception exc) {
            return buffer.toString();
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public GesticonGesture getCopy() {
        return new GesticonGesture(mCharacter, mAnimName, mAnimPath, mCategory, mBlendable, mDuration, mPreparationEnd,
                                   mRetractionStart, mStrokePhaseStart, mStrokePhaseEnd);
    }
}
