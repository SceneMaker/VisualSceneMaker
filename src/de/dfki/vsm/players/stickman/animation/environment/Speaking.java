	/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.animation.environment;

import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animationlogic.Animation;
import de.dfki.vsm.players.stickman.animationlogic.AnimationContent;
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
public class Speaking extends Animation {

	public Speaking(Stickman sm, int duration, boolean block) {
		super(sm, duration, block);
	}

	@Override
	public void playAnimation() {
		if (mParameter instanceof String) {
			mStickman.mSpeechBubble.mText = (String) mParameter;
		}

		mAnimationPart = new ArrayList<>();
		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.SPEAK.name()));
		playAnimationPart(mDuration);

		mAnimationPart.add(new AnimationContent(mStickman.mSpeechBubble, "shape", SpeechBubble.SHAPE.DEFAULT.name()));
		playAnimationPart(20);

	}

//	@Override
//	public void writeXML(IOSIndentWriter out) throws XMLWriteError {
//		out.println("<StickmanAnimation name=\"" + getClass().getSimpleName() + "\" duration=\"" + mDuration + "\" blocking=\"" + mBlocking + "\">").push();
//
//		if (mParameter instanceof String) {
//			out.println((String) mParameter);
//		}
//
//		out.pop().println("</StickmanAnimation>");
//	}
//
//	@Override
//	public final void parseXML(final Element element) throws XMLParseError {
//
//				mName = element.getAttribute("name");
//		mDuration = Integer.parseInt(element.getAttribute("duration"));
//		mBlocking = Boolean.parseBoolean(element.getAttribute("blocking"));
//
//		// Process The Child Nodes
//		XMLParseAction.processChildNodes(element, new XMLParseAction() {
//			@Override
//			public void run(final Element element) throws XMLParseError {
//
//				mParameter = (String)element.getTextContent();
//			}
//		});
//	}
}
