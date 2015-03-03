package de.dfki.vsm.api.hcm;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.IOException;
import java.io.StringReader;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * Data structure for a message exchanged between VSM and the agent proxies.
 * 
 * @author Gregor Mehlmann, Kathrin Janowski
 */
public final class VSMMessage {
    
    private final LOGDefaultLogger mVSM3Log = LOGDefaultLogger.getInstance();

    // The Action Name
    private String mName;
    // The Action Type
    private String mType;
    // The Action Task
    private String mTask;
    // The Action Date
    private String mDate;
    // The Action Time
    private String mTime;
    // The Action Text
    private String mText;
    
    //the default value (used to avoid Null Pointer Exceptions)
    public static final String cUnknownValue = "unknown"; 


    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public VSMMessage(
            final String name,
            final String type,
            final String task,
            final String date,
            final String time,
            final String text) {
        mName = name;
        mType = type;
        mTask = task;
        mDate = date;
        mTime = time;
        mText = text;
    }

    /**
     * Parses an XML message string from a proxy application.
     * 
     * The attributes in the source string can be given in any order,
     * as long as it the String a valid XML element.
     * Missing attributes are set to cUnknownValue.
     * 
     * @param message the XML message
     */
    public VSMMessage(String message)
    {
        try{
            //parse the XML
            DocumentBuilderFactory documentFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = documentFactory.newDocumentBuilder();
            InputSource inputSource = new InputSource();
            inputSource.setCharacterStream(new StringReader(message));
            Document document = documentBuilder.parse(inputSource);
            Element action = document.getDocumentElement();

            mName = action.getAttribute("name");
            mType = action.getAttribute("type");
            mTask = action.getAttribute("task");
            mDate = action.getAttribute("time");
            mTime = action.getAttribute("time");
        
            mText = action.getTextContent();
        }
        catch(ParserConfigurationException pce)
        {
            mVSM3Log.failure("Could not create XML Document Builder: "
                    +pce.getMessage());
        }
        catch (SAXException ex) {
            mVSM3Log.failure("Could not parse XML message: "
                    +ex.getMessage());
        }
        catch (IOException ex) {
            mVSM3Log.failure("Could not create input source for parsing XML message: "
                    +ex.getMessage());
        }
        finally
        {
            //initialize missing values
            if ((mName==null) || mName.isEmpty()) mName = cUnknownValue;
            if ((mType==null) || mType.isEmpty()) mType = cUnknownValue;
            if ((mTask==null) || mTask.isEmpty()) mTask = cUnknownValue;
            if ((mTime==null) || mTime.isEmpty()) mTime = cUnknownValue;
            if (mText == null) mText="";
        }
    }

    
    //==========================================================================
    // getters & setters
    //==========================================================================
    
    /**
     * @return the action's message string representation
     * which is ready to be sent to a VSM proxy
     */
    @Override
    public final String toString() {
        return getMessageString();
    }

    /**
     * @return the action's message string representation
     * which is ready to be sent to a VSM proxy
     */
    public final String getMessageString() {

        return "<action "
                + "name=\"" + mName + "\" "
                + "type=\"" + mType + "\" "
                + "task=\"" + mTask + "\" "
                + "date=\"" + mDate + "\" "
                + "time=\"" + mTime + "\">"
                + mText + "</action>";
    }


    /**
     * @return the name of the agent 
     */
    public final String getName() {
        return mName;
    }

    /**
     * @return the task type 
     */
    public final String getType() {
        return mType;
    }

    /**
     * @return the task ID 
     */
    public final String getTask() {
        return mTask;
    }

    /**
     * @return the date when the task was created  
     */
    public final String getDate() {
        return mDate;
    }

    /**
     * @return the time when the task was created  
     */
    public final String getTime() {
        return mTime;
    }

    /**
     * @return the command sent to the agent
     */
    public final String getText() {
        return mText;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setName(final String name) {
        mName = name;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setType(final String type) {
        mType = type;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setTask(final String task) {
        mTask = task;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setDate(final String date) {
        mDate = date;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setTime(final String time) {
        mTime = time;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setText(final String text) {
        mText = text;
    }
    
    
    //==========================================================================
    // availability of core attributes
    //==========================================================================

    public boolean hasName()
    {
        return !mName.equals(cUnknownValue);
    }

    public boolean hasType()
    {
        return !mTask.equals(cUnknownValue);
    }

    public boolean hasTask()
    {
        return !mTask.equals(cUnknownValue);
    }

    public boolean hasText()
    {
        return !mText.isEmpty();
    }

}
