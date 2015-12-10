package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.runtime.exceptions.InterpretException;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Vector;

/**
 * @author Not me
 */
public class Configuration {
    private final HashMap<Node, LinkedList<State>> mConfiguration = new HashMap<Node, LinkedList<State>>();

    public void clear() {
        mConfiguration.clear();
    }

    public void enterState(State state) {
        if (mConfiguration.get(state.getNode()) == null) {
            mConfiguration.put(state.getNode(), new LinkedList<State>());
        }

        mConfiguration.get(state.getNode()).addLast(state);
    }

    public void exitState(Node state, Process thread) throws InterpretException {
        if (mConfiguration.get(state) == null) {
            throw new InterpretException(this,
                                       "Configuration Error: There is no thread currently executing node " + state);
        }

        Vector<State> removableStateList = new Vector<State>();

        for (State configState : mConfiguration.get(state)) {
            if (configState.getThread().equals(thread)) {
                removableStateList.add(configState);
            }
        }

        if (removableStateList.isEmpty()) {
            throw new InterpretException(this,
                                       "Configuration Error: Thread " + thread.getName() + "(" + thread.getId()
                                       + ") is not currently executing node " + state);
        }

        if (removableStateList.size() > 1) {
            throw new InterpretException(this,
                                       "Configuration Error: Thread " + thread.getName() + "(" + thread.getId()
                                       + ") cannot be executing node " + state + " more than once at one time");
        }

        for (State configState : removableStateList) {
            mConfiguration.get(state).remove(configState);
        }

        if (mConfiguration.get(state).isEmpty()) {
            mConfiguration.remove(state);
        }
    }

    public State getState(Node node) throws InterpretException {
        if (mConfiguration.get(node) == null) {
            throw new InterpretException(this,
                                       "Configuration Error: Node " + node.getId()
                                       + " is currently not executed by any thread");
        }

        if (mConfiguration.get(node).isEmpty()) {
            throw new InterpretException(this,
                                       "Configuration Error: Node " + node.getId()
                                       + " is currently not executed by any thread");
        }

        return mConfiguration.get(node).getLast();
    }

    public State getState(String id) throws InterpretException {
        for (Node node : mConfiguration.keySet()) {
            if (node.getId().equals(id)) {
                return getState(node);
            }
        }

        throw new InterpretException(this,
                                   "Configuration Error: Node " + id + " is currently not executed by any thread");
    }

    // TODO: Get only one configuration state for each node, no double states
    // TODO: Get only supernodes
    public Object[] getOrderedStates() {

        // Make a list with all config states
        Vector<State> configStateList = new Vector<State>();

        for (LinkedList<State> stateVec : mConfiguration.values()) {
            for (State state : stateVec) {
                configStateList.add(state);
            }
        }

        // Make a sorted list of config states
        Object[] sortedConfigStateArray = configStateList.toArray();

        Arrays.sort(sortedConfigStateArray);

        // Return the sorted array
        return sortedConfigStateArray;
    }

    public boolean isInState(String state) {
        for (Node node : mConfiguration.keySet()) {
            if (node.getId().equals(state)) {
                if (mConfiguration.get(node) != null) {
                    if (!mConfiguration.get(node).isEmpty()) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    public static class State implements Comparable, ModelObject {
        private final transient Node    mNode;
        private final transient Process mThread;

        public State(Node node, Process thread) {
            mNode   = node;
            mThread = thread;
        }

        public Node getNode() {
            return mNode;
        }

        public Process getThread() {
            return mThread;
        }

        @Override
        public int compareTo(Object obj) {
            State configState = (State) obj;

            if (mThread.getLevel() > configState.mThread.getLevel()) {
                return 1;
            } else if (mThread.getLevel() < configState.mThread.getLevel()) {
                return -1;
            } else {
                return 0;
            }
        }

        @Override
        public State getCopy() {
            return null;
        }

        @Override
        public void parseXML(Element element) throws XMLParseError {}

        @Override
        public void writeXML(IOSIndentWriter out) {
            out.println("<ConfigState node=\"" + mNode.getId() + "\" thread= \"" + mThread.toString() + "\" level=\""
                        + mThread.getLevel() + "\"/>");
        }
    }
}
