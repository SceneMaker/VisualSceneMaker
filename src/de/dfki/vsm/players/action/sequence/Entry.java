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
public class Entry  {

	public static enum TYPE {

		GENERIC, WORD, TIMEMARK
	};
	
	public String mContent;
	public TYPE mType;
	
	public Entry() {
		mType = TYPE.GENERIC;
	}

	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
	}

	public String toString() {
       return "entry";
	}
}
