package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import de.dfki.vsm.util.xml.XMLWriteError;
import org.w3c.dom.Element;

import java.util.*;

/**
 * @author Gregor Mehlmann
 */
public final class SceneScript extends ScriptEntity {

    // The List Of Entities
    private List<ScriptEntity> mEntityList = new LinkedList<>();

    // The List Of Comments
    private List<SceneComment> mCommentList = new LinkedList<>();

    // The List Of Scenes
    private List<SceneObject> mSceneList = new LinkedList<>();

    // Map Of Scene Groups
    private final Map<String, SceneGroup> mGroupMap = new HashMap<>();

    // Map Of Scene Groups
    private final Map<String, HashMap<String, SceneGroup>> mLangMap = new HashMap<>();

    public SceneScript() {
    }

    public SceneScript(final int lower, final int upper, final List<ScriptEntity> list) {
        super(lower, upper);
        // Initialize The List
        mEntityList = list;
        // Initialize Object Lists
        initObjectLists();
        // Initialize The Groups
        initGroupMap();
        initLangMap();
    }

    private void initObjectLists() {
        // First Clear The Groups
        mSceneList.clear();
        mCommentList.clear();
        //
        for (final ScriptEntity entity : mEntityList) {
            if (entity instanceof SceneObject) {
                mSceneList.add((SceneObject) entity);
            } else if (entity instanceof SceneComment) {
                mCommentList.add((SceneComment) entity);
            } else {
                throw new IllegalStateException("This should not happen" + entity);
                // This Should Not Happen
            }
        }
    }

    private final void initLangMap() {
        // First Clear The Groups
        mLangMap.clear();
        // Initialize The Groups
        for (final SceneObject scene : mSceneList) {
            // Get The Group Name
            final String name = scene.getName();
            final String lang = scene.getLanguage();
            // Add Scene To Group
            mLangMap.computeIfAbsent(lang, k -> new HashMap<>());

            if (mLangMap.get(lang).get(name) == null) {
                mLangMap.get(lang).put(name, new SceneGroup(name));
            }
            mLangMap.get(lang).get(name).add(scene);
        }
    }

    private final void initGroupMap() {
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

    public final Set<String> getLangSet() {
        return mLangMap.keySet();
    }

    public final List<ScriptEntity> getEntityList() {
        return mEntityList;
    }

    public final void setEntityList(final List<ScriptEntity> list) {
        mEntityList = list;
    }

    public final List<SceneObject> getSceneList() {
        return mSceneList;
    }

    public final void setSceneList(final List<SceneObject> list) {
        mSceneList = list;
    }

    public final List<SceneComment> getCommentList() {
        return mCommentList;
    }

    public final void setCommentList(final List<SceneComment> list) {
        mCommentList = list;
    }

    public final List<SceneObject> copySceneList() {
        // Construct A List Copy
        final LinkedList<SceneObject> copy = new LinkedList<>();
        // Copy Each Single Member
        for (final SceneObject scene : mSceneList) {
            copy.add(scene.getCopy());
        }
        // Return The Final Clone
        return copy;
    }

    public final List<SceneComment> copyCommentList() {
        // Construct A List Copy
        final LinkedList<SceneComment> copy = new LinkedList<>();
        // Copy Each Single Member
        for (final SceneComment comment : mCommentList) {
            copy.add(comment.getCopy());
        }
        // Return The Final Clone
        return copy;
    }

    public final List<ScriptEntity> copyEntityList() {
        // Construct A List Copy
        final LinkedList<ScriptEntity> copy = new LinkedList<>();
        // Copy Each Single Member
        for (final ScriptEntity entity : mEntityList) {
            copy.add(entity.getCopy());
        }
        // Return The Final Clone
        return copy;
    }

    public final Map<String, SceneGroup> getSceneGroupMap() {
        return mGroupMap;
    }

    public final TreeSet<SceneGroup> getOrderedGroupSet() {
        return new TreeSet<>(mGroupMap.values());
    }

    public final SceneGroup getSceneGroup(final String name) {
        return mGroupMap.get(name);
    }

    public final SceneGroup getSceneGroup(final String lang, final String name) {
        return mLangMap.get(lang).get(name);
    }

    public final boolean isSceneListEmpty() {
        return mSceneList.isEmpty();
    }

    public final int getSceneListSize() {
        return mSceneList.size();
    }

    @Override
    public final String getText() {
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < mEntityList.size(); i++) {
            result.append(mEntityList.get(i).getText());

            if (i < mEntityList.size() - 1) {
                result.append("\n\n");
            }
        }

        return result.toString();
    }

    @Override
    public final String getText(final HashMap<String, String> args) {
        StringBuilder result = new StringBuilder();

        for (int i = 0; i < mEntityList.size(); i++) {
            result.append(mEntityList.get(i).getText(args));

            if (i < mEntityList.size() - 1) {
                result.append("\n\n");
            }
        }

        return result.toString();
    }

    @Override
    public final SceneScript getCopy() {
        return new SceneScript(mLower, mUpper, copyEntityList());
    }

    @Override
    public final void writeXML(final IOSIndentWriter stream) throws XMLWriteError {
        stream.println("<SceneScript " + "lower=\"" + mLower + "\" " + "upper=\"" + mUpper + "\">");
        stream.push();
        for (final ScriptEntity entity : mEntityList) {
            entity.writeXML(stream);
            stream.endl();
        }
        stream.pop();
        stream.print("</SceneScript>");
        stream.flush();
    }

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
                initObjectLists();
                // Initialize The Group Map
                initLangMap();
                initGroupMap();
            }
        });
    }

    // Parse the scene script
    public final boolean parseTXT(final String text) {
        // Parse Content Into Scene Script
        final SceneScript script = (SceneScript)ScriptParser.run(text, false, false, true, false, false);

        // Copy Content If Successfully
        if (script != null) {
            // Initialize The Scene List
            mEntityList = script.getEntityList();
            // Initialize The Two Lists
            initObjectLists();
            // Initialize The Group Map
            initLangMap();
            initGroupMap();
            //
            return true;
        } else {
            // Why has this been commented???
            /*
           mEntityList.clear();
            mSceneList.clear();
            mCommentList.clear();
            mGroupMap.clear();
            mLangMap.clear();
             */
            return false;
        }
    }

    public int getHashCode() {
        int hashCode = ((mEntityList == null)
                ? 0
                : getText().hashCode());
        return hashCode;
    }
}
