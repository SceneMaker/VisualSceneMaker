/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.action.sequence;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLWriteError;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class TimeMark extends Entry {

	public TimeMark(String content) {
		mType = TYPE.TIMEMARK;
		mContent = content;
	}

	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
		out.println("<TimeMarkEntry>").push();
		out.println(mContent);
		out.pop().println("</TimeMarkEntry>");

	}
}
