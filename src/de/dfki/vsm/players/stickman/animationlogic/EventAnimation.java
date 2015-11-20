/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animationlogic;

import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.List;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class EventAnimation extends Animation {

	public SceneUttr mUtterance;
	public List<Long> mTimepoints;
	public WordTimeMarkSequence mWTS;
	
	public EventAnimation() {
		super();
	}

	public EventAnimation(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
		mName = getClass().getSimpleName();
		setName(sm.mName + "'s Event Animation " + mName);
	}

	public void playEventAnimationPart() {
		mAnimator = new Animator(mStickman, this, mAnimationPart, mWTS);

		try {
			mAnimationPartStart.acquire();
		} catch (InterruptedException ex) {
			mStickman.mLogger.severe(ex.getMessage());
		}
	}
	
	@Override
	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
		out.println("<StickmanEventAnimation stickmanname = \"" + mStickmanName + "\" name=\"" + mName + "\" id=\"" + mID + "\" duration=\"" + mDuration + "\" blocking=\"" + mBlocking + "\">").push();
		if (mParameter != null) {

			if (mParameter instanceof WordTimeMarkSequence) {
				((WordTimeMarkSequence) mParameter).writeXML(out);
			}

			if (mParameter instanceof String) {
				out.println((String) mParameter);
			}
		}
		out.pop().println("</StickmanEventAnimation>");
	}

	@Override
	public void parseXML(final Element element) throws XMLParseError {

		mStickmanName = element.getAttribute("stickmanname");
		mName = element.getAttribute("name");
		mID = element.getAttribute("id");
		mDuration = Integer.parseInt(element.getAttribute("duration"));
		mBlocking = Boolean.parseBoolean(element.getAttribute("blocking"));

		// Process The Child Nodes
		XMLParseAction.processChildNodes(element, new XMLParseAction() {
			@Override
			public void run(final Element element) throws XMLParseError {

				// Get The Child Tag Name
				final String name = element.getTagName();

				if (name.equalsIgnoreCase("WordTimeMarkSequence")) {
					mParameter = new WordTimeMarkSequence();

					((WordTimeMarkSequence) mParameter).parseXML(element);
				} else {
					mParameter = (String) element.getTextContent();
				}
			}
		});
	}
}