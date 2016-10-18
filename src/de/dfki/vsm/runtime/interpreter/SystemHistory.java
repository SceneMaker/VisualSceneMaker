package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.runtime.interpreter.symbol.SymbolTable;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.util.cpy.Copyable;
import java.util.ArrayList;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashMap;
import java.util.LinkedList;

/**
 * @author Gregor Mehlmann
 */
public class SystemHistory {
    private final HashMap<String, Stack> mHistoryStackMap = new HashMap<String, Stack>();
    private final HashMap<String, Entry> mHistoryEntryMap = new HashMap<String, Entry>();

    public void push(BasicNode node, Entry entry) {
        if (mHistoryStackMap.get(node.getId()) == null) {
            mHistoryStackMap.put(node.getId(), new Stack(node));
        }

        mHistoryStackMap.get(node.getId()).push(entry);
    }

    public void pop(BasicNode node, Entry entry) {
        mHistoryStackMap.get(node.getId()).pop();
    }

    public Entry get(BasicNode node, int index) {
        if (mHistoryStackMap.get(node.getId()) == null) {
            return null;

            // TODO: throw RunTimeException
        } else {
            return mHistoryStackMap.get(node.getId()).get(index);
        }
    }

    // TODO:
    public Entry get(String id, int index) {
        //System.err.println(id);
        //System.err.println(index);

        return mHistoryStackMap.get(id).get(index);
    }

    public void setDepth(BasicNode node, int depth) {
        mHistoryStackMap.get(node.getId()).setDepth(depth);
    }

    public void erase(BasicNode node) {
        mHistoryStackMap.remove(node.getId());
    }

    // TODO:
    public void deepErase(BasicNode id) {}

    // TODO:
    public void setDepth(String state, int depth) {}

    // TODO:
    public void erase(String id) {}

    // TODO:
    public void deepErase(String id) {}

    public void clear() {
        mHistoryStackMap.clear();
        mHistoryEntryMap.clear();
    }

    public boolean isEmpty(BasicNode node) {
        if (mHistoryStackMap.get(node.getId()) == null) {
            return true;
        } else {
            if (mHistoryStackMap.get(node.getId()).isEmpty()) {
                return true;
            }
        }

        return false;
    }

    ////////////////////////////
    public Entry get(BasicNode node) {
        return mHistoryEntryMap.get(node.getId());
    }

    public void set(BasicNode node, Entry entry) {
        mHistoryEntryMap.put(node.getId(), entry);
    }

//  ////////////////
    public static class Entry implements Copyable {
        private final HashMap<String, BasicNode> mChildNodeMap = new HashMap<String, BasicNode>();
        private final ArrayList<Command>       mCommandList  = new ArrayList<Command>();
        private final BasicNode                  mNode;
        private SymbolTable                 mSymbolTable;
        private final long                  mStartTime;
        private long                        mEndTime;

        public Entry(BasicNode node) {
            mNode      = node;
            mStartTime = System.currentTimeMillis();
        }

        public void setEndTime() {
            mEndTime = System.currentTimeMillis();
        }

        public long getRunTime() {
            return mEndTime - mStartTime;
        }

        public void addCmd(Command value) {
            mCommandList.add(value);
        }

        public void addChildNode(BasicNode node) {
            mChildNodeMap.put(node.getId(), node);
        }

        public AbstractValue getValueOf(String var) {
            return mSymbolTable.read(var);
        }

        public void setSymbolTable(SymbolTable symbolTable) {
            mSymbolTable = symbolTable.getCopy();
        }

        public boolean containsChildNode(String id) {
            return mChildNodeMap.containsKey(id);
        }

        public Entry getCopy() {
            return null;
        }
    }


    public class Stack {
        private final LinkedList<Entry> mHistoryStack = new LinkedList<Entry>();
        int                             mDepth        = 1;
        private final BasicNode              mNode;

        public Stack(BasicNode node) {
            mNode = node;
        }

        public BasicNode getNode() {
            return mNode;
        }

        public String getNodeId() {
            return mNode.getId();
        }

        public void push(Entry entry) {
            if (mHistoryStack.size() >= mDepth) {
                mHistoryStack.removeLast();
            }

            mHistoryStack.push(entry);
        }

        public Entry pop() {
            return mHistoryStack.pop();
        }

        public Entry getFirst() {
            return mHistoryStack.getFirst();
        }

        public Entry getLast() {
            return mHistoryStack.getLast();
        }

        public Entry get(int index) {
            try {
                return mHistoryStack.get(index);
            } catch (IndexOutOfBoundsException e) {
                return null;
            }
        }

        public boolean isEmpty() {
            return mHistoryStack.isEmpty();
        }

        public void clear() {
            mHistoryStack.clear();
        }

        public void setDepth(int value) {
            mDepth = value;
        }
    }
}
