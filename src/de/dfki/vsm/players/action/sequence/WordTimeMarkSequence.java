/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.action.sequence;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLParseable;
import de.dfki.vsm.util.xml.XMLWriteError;
import de.dfki.vsm.util.xml.XMLWriteable;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class WordTimeMarkSequence implements XMLParseable, XMLWriteable{

	protected String mText = "";
	protected ArrayList<Entry> mWordsAndTimemarks;
	// The singelton logger instance
	private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

	public WordTimeMarkSequence() {
		mWordsAndTimemarks = new ArrayList<>();
	}

	public WordTimeMarkSequence(String text) {
		mText = text;
		mWordsAndTimemarks = new ArrayList<>();
	}

	public void add(Entry e) {
		boolean add = true;
		synchronized (mWordsAndTimemarks) {
			for (Entry entry : mWordsAndTimemarks) {
				if (entry.mType == Entry.TYPE.TIMEMARK && e.mType == Entry.TYPE.TIMEMARK) {
					if (entry.mContent.equalsIgnoreCase(e.mContent)) {
						add = false;
						break;
					}
				}
			}
			// only add unique timemarks
			if (add) {
				mWordsAndTimemarks.add(e);
			}
		}

	}

	public String getText() {
		return mText;
	}

	public ArrayList<Entry> getSequence() {
		return mWordsAndTimemarks;
	}

	public int getNumberofClusters() {
		int clusterCnt = 0;

		Entry lastEntry = new Entry();
		for (Entry e : mWordsAndTimemarks) {
			clusterCnt = (e.mType != lastEntry.mType) ? clusterCnt + 1 : clusterCnt;
			lastEntry = e;
		}

		return clusterCnt;
	}

	public ArrayList<ArrayList<Entry>> getClusters() {
		ArrayList<ArrayList<Entry>> clusters = new ArrayList<>();

		Entry lastEntry = new Entry();
		ArrayList<Entry> cluster = new ArrayList<>();
		for (Entry e : mWordsAndTimemarks) {
			if (e.mType != lastEntry.mType) {
				// if there is a cluster with entries add it to the clusters
				if (cluster.size() > 0) {
					clusters.add(cluster);
				}
				cluster = new ArrayList<>();
				cluster.add(e);
				lastEntry = e;
			} else { // (e.mType == lastEntry.mType) {
				cluster.add(e);
			}
		}
		// if there is a last cluster with entries add it to the clusters
		if (cluster.size() > 0) {
			clusters.add(cluster);
		}

		return clusters;
	}

	public static Entry.TYPE getClusterType(ArrayList<Entry> cluster) {
		Entry.TYPE type = Entry.TYPE.GENERIC;

		for (Entry e : cluster) {
			type = (e.mType == Entry.TYPE.WORD && type == Entry.TYPE.GENERIC) ? Entry.TYPE.WORD : type;
			type = (e.mType == Entry.TYPE.TIMEMARK && type == Entry.TYPE.GENERIC) ? Entry.TYPE.TIMEMARK : type;
		}

		return type;
	}
	
	
	@Override
	public final void parseXML(final Element element) throws XMLParseError {

		mText = element.getAttribute("text");

		// Process The Child Nodes
		XMLParseAction.processChildNodes(element, new XMLParseAction() {
			@Override
			public void run(final Element element) throws XMLParseError {

				// Get The Child Tag Name
				final String name = element.getTagName();
				
				// Check The Child Tag Name
				if (name.equals("Entries")) {
					XMLParseAction.processChildNodes(element, new XMLParseAction() {
						@Override
						public void run(Element element) throws XMLParseError {							
							// Get The Child Tag Name
							final String name = element.getTagName();
							
							if (name.equalsIgnoreCase("WordEntry")) {
								Word word = new Word();
								
								word.parseXML(element);
								
								mWordsAndTimemarks.add(word);
							}
							
							if (name.equalsIgnoreCase("TimeMarkEntry")) {
								TimeMark timemark = new TimeMark();
								
								timemark.parseXML(element);
								
								mWordsAndTimemarks.add(timemark);
							}
						}
					});
				}
			}
		});
	}

	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
		out.println("<WordTimeMarkSequence text=\"" + mText + "\">").push();

		out.println("<Entries>").push();
		for (Entry e : mWordsAndTimemarks) {
			e.writeXML(out);
		}
		out.pop().println("</Entries>");

		out.pop().println("</WordTimeMarkSequence>");
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();

		sb.append("WordTimeMarkSequence for ").append(mText).append("\n");
		sb.append("\t");
		for (Entry e : mWordsAndTimemarks) {
			sb.append(e).append(",");
		}
		sb.deleteCharAt(sb.length() - 1);
		sb.append("\n");
		return sb.toString();
	}
}
