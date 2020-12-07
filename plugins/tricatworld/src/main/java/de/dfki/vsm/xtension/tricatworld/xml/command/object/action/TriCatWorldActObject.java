package de.dfki.vsm.xtension.tricatworld.xml.command.object.action;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 */
public class TriCatWorldActObject implements XMLParseable, XMLWriteable {

    protected String mName = "";
    protected String mId = "";

    public void setId(String id) {
        mId = id;
    }

    public String getId() {
        return mId;
    }

    public String getActionCmd() {
        return mName;
    }

    public void resetActionCmd(String newcmdname) {
        mName = newcmdname;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
    }
}
