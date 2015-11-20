/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.environment.event;

import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.AnimationContent;
import de.dfki.vsm.players.stickman.animationlogic.EventAnimation;
import de.dfki.vsm.players.stickman.environment.SpeechBubble;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import java.util.ArrayList;
import org.w3c.dom.Element;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class Speaking extends EventAnimation {
	
	public Speaking() {
		super();
	}

	public Speaking(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		if (mParameter instanceof WordTimeMarkSequence) {
			mWTS = (WordTimeMarkSequence) mParameter;
		}

		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.SPEAK.name(), mWTS));

		playEventAnimationPart();

		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.DEFAULT.name()));
		playAnimationPart(20);

	}

//	@Override
//	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
//		out.println("<StickmanAnimation name=\"" + getClass().getSimpleName() + "\" duration=\"" + mDuration + "\" blocking=\"" + mBlocking + "\">").push();
//
//		if (mParameter instanceof WordTimeMarkSequence) {
//			((WordTimeMarkSequence)mParameter).writeXML(out);
//		}
//		
//		if (mParameter instanceof String) {
//			out.println((String) mParameter);
//		}
//
//		out.pop().println("</StickmanAnimation>");
//	}

//	@Override
//	public final void parseXML(final Element element) throws XMLParseError {
//
//		mName = element.getAttribute("name");
//		mDuration = Integer.parseInt(element.getAttribute("duration"));
//		mBlocking = Boolean.parseBoolean(element.getAttribute("blocking"));
//
//		// Process The Child Nodes
//		XMLParseAction.processChildNodes(element, new XMLParseAction() {
//			@Override
//			public void run(final Element element) throws XMLParseError {
//
//				// Get The Child Tag Name
//				final String name = element.getTagName();
//
//				if (name.equalsIgnoreCase("WordTimeMarkSequence")) {
//					mWTS = new WordTimeMarkSequence();
//					mParameter = mWTS;
//
//					mWTS.parseXML(element);
//				} else {
//					mParameter = element.getTextContent();
//				}
//			}
//		});
//	}
}
