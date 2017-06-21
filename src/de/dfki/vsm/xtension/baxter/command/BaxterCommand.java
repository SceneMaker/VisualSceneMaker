package de.dfki.vsm.xtension.baxter.command;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import org.w3c.dom.Element;

import java.util.ArrayList;

/**
 * Created by alvaro on 2/23/16.
 */
public class BaxterCommand implements XMLParseable, XMLWriteable {

    private String mName;
    private String mID;
    private ArrayList<String> mParams;
    public BaxterCommand(String name, String Id, ArrayList<String> params){
        mName = name;
        mID = Id;
        if(params == null){
            mParams = new ArrayList<>();
        }
        mParams = params;
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {

    }



    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.println("<Command method=\"" + mName + "\" id=\"" + mID + "\">").push();
        out.println("<params>").push();
        for (String param: mParams ) {
            out.println("<param>").push();
            out.println(param).push();
            out.println("</param>").push();
        }
        out.println("</params>").push();
        out.pop().pop().println("</Command>");
    }
}
