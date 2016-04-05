/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.command;

import de.dfki.charactor.v4235.AnimationTrack;
import de.dfki.charactor.v4235.ComplexAnimationGenerator;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.util.LinkedList;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class TWorldCommandObjectCharamelSpeakAction extends TWorldCommandObjectAction implements XMLParseable, XMLWriteable {

    private final LinkedList mTextBlocks;
    private final String mPunctuation;
    private final int mCharamelCharacterId = 2;
    // The logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // TODO cai_request sub element String mValue = "";
    public TWorldCommandObjectCharamelSpeakAction(LinkedList textblocks, String punct) {
        mName = "caixml";
        mTextBlocks = textblocks;
        mPunctuation = punct;
    }

    public TWorldCommandObjectCharamelSpeakAction() {
        mName = "caixml";
        mTextBlocks = new LinkedList();
        mPunctuation = "";
    }

    private final String buildUtterance() {
        final StringBuilder builder = new StringBuilder();
        for (final Object item : mTextBlocks) {
            builder.append(item.toString());
            if (!item.equals(mTextBlocks.getLast())) {
                builder.append(' ');
            } else {
                builder.append(mPunctuation);
            }
        }
        return builder.toString();
    }
    
    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\">").push();

        ComplexAnimationGenerator ca = new ComplexAnimationGenerator();
        AnimationTrack track1 = ca.addTrack();
        track1.addSpeakText(mCharamelCharacterId, buildUtterance());
        out.push().println(ca.getCaiXML());
        out.pop().pop().println("</Action>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        // TODO parse sub elements cai_request mValue = element.getAttribute("value");
    }
}
