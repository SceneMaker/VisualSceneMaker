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
import java.util.TreeSet;

/**
 * @author Not me
 */
public final class SceneScript extends SceneEntity {

    // The List Of Entities
    private LinkedList<SceneEntity> mEntityList = new LinkedList<>();

    // The List Of Comments
    private LinkedList<SceneComment> mCommentList = new LinkedList<>();

    // The List Of Scenes
    private LinkedList<SceneObject> mSceneList = new LinkedList<>();

    // Map Of Scene Groups
    private final HashMap<String, SceneGroup> mGroupMap = new HashMap<>();

    // Map Of Scene Groups
    private final HashMap<String, HashMap<String, SceneGroup>> mLangMap = new HashMap<>();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SceneScript() {
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SceneScript(final int lower, final int upper, final LinkedList<SceneEntity> list) {
        super(lower, upper);

        // Initialize The List
        mEntityList = list;

        // Initialize Object Lists
        initObjectLists();

        // Initialize The Groups
        initGroupMap();
        initLangMap();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private void initObjectLists() {

        // First Clear The Groups
        mSceneList.clear();
        mCommentList.clear();

        //
        for (final SceneEntity entity : mEntityList) {
            if (entity instanceof SceneObject) {
                mSceneList.add((SceneObject) entity);
            } else if (entity instanceof SceneObject) {
                mCommentList.add((SceneComment) entity);
            } else {

                // This Should Not Happen
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void initLangMap() {

        // First Clear The Groups
        mLangMap.clear();

        // Initialize The Groups
        for (final SceneObject scene : mSceneList) {

            // Get The Group Name
            final String name = scene.getName();
            final String lang = scene.getLanguage();

            // Add Scene To Group
            if (mLangMap.get(lang) == null) {
                mLangMap.put(lang, new HashMap<String, SceneGroup>());
            }

            if (mLangMap.get(lang).get(name) == null) {
                mLangMap.get(lang).put(name, new SceneGroup(name));
            }

            mLangMap.get(lang).get(name).add(scene);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void initGroupMap() {

        // First Clear The Groups
        mGroupMap.clear();

        // Initialize The Groups
        for (final SceneObject scene : mSceneList) {

            // Get The Group Name
            final String name = scene.getName();

            // Add Scene To Group
            if (mGroupMap.get(name) == null) {
                mGroupMap.put(name, new SceneGroup(name));
            }

            mGroupMap.get(name).add(scene);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneEntity> getEntityList() {
        return mEntityList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setEntityList(final LinkedList<SceneEntity> list) {
        mEntityList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneObject> getSceneList() {
        return mSceneList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setSceneList(final LinkedList<SceneObject> list) {
        mSceneList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneComment> getCommentList() {
        return mCommentList;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void setCommentList(final LinkedList<SceneComment> list) {
        mCommentList = list;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneObject> copySceneList() {

        // Construct A List Copy
        final LinkedList<SceneObject> copy = new LinkedList<>();

        // Copy Each Single Member
        for (final SceneObject scene : mSceneList) {
            copy.add(scene.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneComment> copyCommentList() {

        // Construct A List Copy
        final LinkedList<SceneComment> copy = new LinkedList<>();

        // Copy Each Single Member
        for (final SceneComment comment : mCommentList) {
            copy.add(comment.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final LinkedList<SceneEntity> copyEntityList() {

        // Construct A List Copy
        final LinkedList<SceneEntity> copy = new LinkedList<>();

        // Copy Each Single Member
        for (final SceneEntity entity : mEntityList) {
            copy.add(entity.getCopy());
        }

        // Return The Final Clone
        return copy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final HashMap<String, SceneGroup> getSceneGroupMap() {
        return mGroupMap;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final TreeSet<SceneGroup> getOrderedGroupSet() {
        return new TreeSet(mGroupMap.values());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final SceneGroup getSceneGroup(final String name) {
        return mGroupMap.get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final SceneGroup getSceneGroup(final String lang, final String name) {
        return mLangMap.get(lang).get(name);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final boolean isSceneListEmpty() {
        return mSceneList.isEmpty();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final int getSceneListSize() {
        return mSceneList.size();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String getText() {
        String result = "";

        for (int i = 0; i < mEntityList.size(); i++) {
            result += mEntityList.get(i).getText();

            if (i < mEntityList.size() - 1) {
                result += "\n\n";
            }
        }

        return result;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final String getText(final HashMap<String, String> args) {
        String result = "";

        for (int i = 0; i < mEntityList.size(); i++) {
            result += mEntityList.get(i).getText(args);

            if (i < mEntityList.size() - 1) {
                result += "\n\n";
            }
        }

        return result;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final SceneScript getCopy() {
        return new SceneScript(mLower, mUpper, copyEntityList());
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<SceneScript " + "lower=\"" + mLower + "\" " + "upper=\"" + mUpper + "\">");
        // + "length=\""
        // + mCommentList.size() + "\">");
        stream.push();
        for (final SceneEntity entity : mEntityList) {
            entity.writeXML(stream);
            stream.endl();
        }
        stream.pop();
        stream.print("</SceneScript>");
        stream.flush();
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    public final void parseXML(final Element element) throws XMLParseError {

        // Parse The Boundary
        mLower = Integer.parseInt(element.getAttribute("lower"));
        mUpper = Integer.parseInt(element.getAttribute("upper"));

        // Process The Child Nodes
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {

                // Get The Child Tag Name
                final String name = element.getTagName();

                // Check The Child Tag Name
                if (name.equals("SceneObject")) {

                    // Create A New Token Style
                    final SceneObject entity = new SceneObject();

                    // Parse The New Token Style
                    entity.parseXML(element);

                    // Put The New Style To The Map
                    mEntityList.add(entity);
                } else if (name.equals("SceneComment")) {

                    // Create A New Token Style
                    final SceneComment entity = new SceneComment();

                    // Parse The New Token Style
                    entity.parseXML(element);

                    // Put The New Style To The Map
                    mEntityList.add(entity);
                } else {

                    // This Should Not Happen
                }

                // Initialize The Two Lists
                // initObjectLists();
                // Initialize The Group Map
                // initLangMap();
                // initGroupMap();
            }
        });
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final void parseTXT(final String text) {

        // Parse Content Into Scene Script
        final SceneScript script = ScriptParser.run("", text, false, true, false, false);

        // Copy Content If Successfully
        if (script != null) {

            // Initialize The Scene List
            mEntityList = script.getEntityList();

            // Initialize The Two Lists
            initObjectLists();

            // Initialize The Group Map
            initLangMap();
            initGroupMap();
        } else {
            mEntityList.clear();
            mSceneList.clear();
            mCommentList.clear();
            mGroupMap.clear();
            mLangMap.clear();
        }
    }
}
