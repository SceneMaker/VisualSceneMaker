/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.action.sequence;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
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
public class TimeMark extends Entry implements XMLParseable, XMLWriteable {

	public TimeMark() {
		mType = TYPE.TIMEMARK;
	}

	public TimeMark(String content) {
		mType = TYPE.TIMEMARK;
		mContent = content;
	}

	@Override
	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
		out.println("<TimeMarkEntry>").push();
		out.println(mContent);
		out.pop().println("</TimeMarkEntry>");
	}
	
	public final void parseXML(final Element element) throws XMLParseError {
		// Process The Child Nodes
		XMLParseAction.processChildNodes(element, new XMLParseAction() {
			@Override
			public void run(final Element element) throws XMLParseError {

				// this is (should be text), so ...
				mContent = element.getTextContent();
			}
		});
	}

	@Override
	public String toString() {
		return mContent;
	}
}
