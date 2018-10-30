package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;
import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public final class SceneObject extends ScriptEntity {

    // The List Of Scene Turns
    private LinkedList<SceneTurn> mTurnList = new LinkedList<>();
    // The Language Suffix
    private String mSceneLang;
    // The Scene Identifier
    private String mSceneName;

    public SceneObject() {
        mSceneLang = null;
        mSceneName = null;
    }

    public SceneObject(
            final int lower,
            final int upper,
            final String lang,
            final String name,
            final LinkedList<SceneTurn> body) {
        super(lower, upper);
        // Initialize Members
        mSceneLang = lang;
        mSceneName = name;
        mTurnList = body;
    }

    public final String getLanguage() {
        return mSceneLang;
    }

    public final void setLanguage(final String language) {
        mSceneLang = language;
    }

    public final String getName() {
        return mSceneName;
    }

    public final void setName(final String name) {
        mSceneName = name;
    }

    public final LinkedList<SceneTurn> getTurnList() {
        return mTurnList;
    }

    public final void setTurnList(final LinkedList<SceneTurn> body) {
        mTurnList = body;
    }

    public final LinkedList<SceneTurn> copyTurnList() {
        // Construct A List Copy
        final LinkedList<SceneTurn> copy = new LinkedList<>();
        // Copy Each Single Member
        for (final SceneTurn turn : mTurnList) {
            copy.add(turn.getCopy());
        }
        // Return The Final Clone
        return copy;
    }

    @Override
    public final String getText() {
        String result = "scene" + " " + mSceneLang + " " + mSceneName + "\n";
        if (mTurnList != null) {
            for (int i = 0; i < mTurnList.size(); i++) {
                result += mTurnList.get(i).getText();

                if (i < mTurnList.size() - 1) {
                    result += "\n";
                }
            }
        }

        return result;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String getText(final HashMap<String, String> args) {
        String result = "scene" + " " + mSceneLang + " " + mSceneName + "\n";
        if (mTurnList != null) {
            for (int i = 0; i < mTurnList.size(); i++) {
                result += mTurnList.get(i).getText(args);

                if (i < mTurnList.size() - 1) {
                    result += "\n";
                }
            }
        }

        return result;
    }

    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<SceneObject "
                + "lower=\"" + mLower + "\" "
                + "upper=\"" + mUpper + "\" "
                + "language=\"" + mSceneLang + "\" "
                + "identifier=\"" + mSceneName + "\">");
        stream.push();
        for (final SceneTurn turn : mTurnList) {
            turn.writeXML(stream);
        }
        stream.pop();
        stream.endl();
        stream.print("</SceneObject>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        // Parse The Boundary
        mLower = Integer.parseInt(element.getAttribute("lower"));
        mUpper = Integer.parseInt(element.getAttribute("upper"));
        // Parse The Text Content
        mSceneLang = element.getAttribute("language");
        mSceneName = element.getAttribute("identifier");
        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                // Create A New Token Style
                final SceneTurn turn = new SceneTurn();
                // Parse The New Token Style
                turn.parseXML(element);
                // Put The New Style To The Map
                mTurnList.add(turn);
            }
        });
    }

    @Override
    public final SceneObject getCopy() {
        return new SceneObject(mLower, mUpper, mSceneLang, mSceneName, copyTurnList());
    }
}