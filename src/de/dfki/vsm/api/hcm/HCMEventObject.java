package de.dfki.vsm.api.hcm;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.util.log.LOGDefaultLogger;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.io.ByteArrayInputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

/**
 * @author Not me
 * @author Kathrin Janowski
 */
public final class HCMEventObject {

    // The VSM Logger
    private static LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    // The Message Id
    private static long sId = 0;

    // The Action Type
    private String mType;

    // The Request Task
    private String mUtid;

    // The Agent Name
    private String mName;

    // The Agent Uaid
    private String mUaid;

    // The Action Text
    private String mText;

    // The Action Umid
    private long mUmid;

    // The Action Date
    private long mDate;

    // The Action Time
    private long mTime;

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private HCMEventObject(final String type, final String utid, final String name, final String uaid,
                            final String text, final long umid, final long date, final long time) {
        mType = type;
        mUtid = utid;
        mName = name;
        mUaid = uaid;
        mText = text;
        mUmid = umid;
        mDate = date;
        mTime = time;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private static synchronized long newId() {

        // Return New Id
        return (++sId);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static HCMEventObject newInstance(final String type, final String utid, final String name,
            final String uaid, final String text, final long date, final long time) {

        // Get New Message Id
        final long umid = newId();

        // Return New Message
        return new HCMEventObject(type, utid, name, uaid, text, umid, date, time);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public static HCMEventObject getInstance(final String input) {
        try {

            // Parse the XML String
            final ByteArrayInputStream   stream   = new ByteArrayInputStream(input.getBytes("UTF-8"));
            final DocumentBuilderFactory factory  = DocumentBuilderFactory.newInstance();
            final DocumentBuilder        builder  = factory.newDocumentBuilder();
            final Document               document = builder.parse(stream);

            // Get The Root Element
            final Element action = document.getDocumentElement();

            // Get The Attributes
            final String type = action.getAttribute("type");
            final String utid = action.getAttribute("utid");
            final String name = action.getAttribute("name");
            final String uaid = action.getAttribute("uaid");
            final String umid = action.getAttribute("umid");
            final String date = action.getAttribute("date");
            final String time = action.getAttribute("time");
            final String text = action.getTextContent();

            // Construct The Message
            final HCMEventObject message = new HCMEventObject(type, utid, name, uaid, text, Long.parseLong(umid),
                                                Long.parseLong(date), Long.parseLong(time));

            // Return The Message
            return message;
        } catch (final Exception exc) {
            sLogger.failure(exc.toString());
        }

        // Return Null Otherwise
        return null;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String toString() {

        // Create The New Command
        return "<action " + "type=\"" + mType + "\" " + "utid=\"" + mUtid + "\" " + "name=\"" + mName + "\" "
               + "uaid=\"" + mUaid + "\" " + "umid=\"" + mUaid + "\" " + "date=\"" + mDate + "\" " + "time=\"" + mTime
               + "\">" + mText + "</action>";
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final String getType() {
        return mType;
    }

    public final String getUtid() {
        return mUtid;
    }

    public final String getName() {
        return mName;
    }

    public final String getUaid() {
        return mUaid;
    }

    public final long getUmid() {
        return mUmid;
    }

    public final long getDate() {
        return mDate;
    }

    public final long getTime() {
        return mTime;
    }

    public final String getText() {
        return mText;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setType(final String type) {
        mType = type;
    }

    public final void setUtid(final String utid) {
        mUtid = utid;
    }

    public final void setName(final String name) {
        mName = name;
    }

    public final void setUaid(final String uaid) {
        mUaid = uaid;
    }

    public final void setUmid(final long umid) {
        mUmid = umid;
    }

    public final void setDate(final long date) {
        mDate = date;
    }

    public final void setTime(final long time) {
        mTime = time;
    }

    public final void setText(final String text) {
        mText = text;
    }
}
