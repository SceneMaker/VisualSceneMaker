package de.dfki.vsm.model.scenescript;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class SceneTurn extends ScriptEntity {

	// The Utterance List
	private LinkedList<SceneUttr> mUttrList = new LinkedList<>();

	// The Turn Speaker
	private String mSpeaker;

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public SceneTurn() {
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public SceneTurn(final int lower, final int upper, final String speaker, final LinkedList<SceneUttr> list) {
		super(lower, upper);

		// Initialize Members
		mSpeaker = speaker;
		mUttrList = list;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final String getSpeaker() {
		return mSpeaker;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final void setSpeaker(final String speaker) {
		mSpeaker = speaker;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final LinkedList<SceneUttr> getUttrList() {
		return mUttrList;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final void setUttrList(final LinkedList<SceneUttr> list) {
		mUttrList = list;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final LinkedList<SceneUttr> copyUttrList() {

		// Construct A List Copy
		final LinkedList<SceneUttr> copy = new LinkedList<>();

		// Copy Each Single Member
		for (final SceneUttr scene : mUttrList) {
			copy.add(scene.getCopy());
		}

		// Return The Final Clone
		return copy;
	}

	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	public final String getCleanText() {
		StringBuilder result = new StringBuilder();

		for (SceneUttr utt : mUttrList) {

			for (UttrElement word : utt.getWordList()) {
				if (word instanceof SceneWord) {
					result.append(word.getText()).append(" ");
				}
			}
			result = new StringBuilder(result.toString().trim() + utt.getPunctuationMark() + " ");
		}

		result = new StringBuilder(result.toString().trim());

		return result.toString();
	}

    ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	@Override
	public final String getText() {
		StringBuilder result = new StringBuilder(mSpeaker + ":");

		for (SceneUttr utt : mUttrList) {
			result.append(utt.getText());
		}

		return result.toString();
	}

    ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	@Override
	public final String getText(final HashMap<String, String> args
	) {
		StringBuilder result = new StringBuilder(mSpeaker + ":");

		for (SceneUttr utt : mUttrList) {
			result.append(utt.getText(args));
		}

		return result.toString();
	}

    ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	@Override
	public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
		stream.println("<SceneTurn " + "lower=\"" + mLower + "\" " + "upper=\"" + mUpper + "\" " + "speaker=\""
		  + mSpeaker + "\">");
		stream.push();

		for (final SceneUttr uttr : mUttrList) {
			uttr.writeXML(stream);

			if (!uttr.equals(mUttrList.getLast())) {
				stream.endl();
			}
		}

		stream.pop();
		stream.endl();
		stream.print("</SceneTurn>");
	}

    ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	@Override
	public final void parseXML(final Element element) throws XMLParseError {

		// Parse The Boundary
		mLower = Integer.parseInt(element.getAttribute("lower"));
		mUpper = Integer.parseInt(element.getAttribute("upper"));

		// Parse The Text Content
		mSpeaker = element.getAttribute("speaker");

		// Process The Child Nodes
		XMLParseAction.processChildNodes(element, new XMLParseAction() {
			@Override
			public void run(Element element) throws XMLParseError {

				// Create A New Token Style
				final SceneUttr uttr = new SceneUttr();

				// Parse The New Token Style
				uttr.parseXML(element);

				// Put The New Style To The Map
				mUttrList.add(uttr);
			}
		});
	}

    ////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////
	@Override
	public final SceneTurn getCopy() {
		return new SceneTurn(mLower, mUpper, mSpeaker, copyUttrList());
	}
}
