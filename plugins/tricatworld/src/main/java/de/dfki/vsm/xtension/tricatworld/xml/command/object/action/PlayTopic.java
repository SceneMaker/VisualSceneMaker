/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
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
 *
 */
public class PlayTopic extends TriCatWorldActObject implements XMLParseable, XMLWriteable {

    private String mTopicId = "";
    private String mPlayQuestion = "";
    private String mPlayAnswer = "";

    public PlayTopic(String topicId, String playQuestion, String playAnswer) {
        mName = "playtopic";
        mTopicId = topicId;
        mPlayQuestion = playQuestion;
        mPlayAnswer = playAnswer;
    }

    @Override
    public void writeXML(IOSIndentWriter out) throws XMLWriteError {
        out.push().println("<Action name=\"" + mName + "\" id=\"" + mId + "\" topicid=\"" + mTopicId + "\" playquestion=\"" + mPlayQuestion + "\" playanswer=\"" + mPlayAnswer + "\"/>");
    }

    @Override
    public void parseXML(final Element element) throws XMLParseError {
        mName = element.getAttribute("name");
        mId = element.getAttribute("id");
        mTopicId = element.getAttribute("topicid");
        mPlayQuestion = element.getAttribute("playquestion");
        mPlayAnswer = element.getAttribute("playanswer");
    }
}
