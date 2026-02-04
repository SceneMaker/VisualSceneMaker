package de.dfki.vsm.runtime.interpreter.value;

import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * A value type representing a consumable FIFO event queue.
 *
 * Writing enqueues values onto the queue. Reading in edge conditions
 * checks if the queue is non-empty: returns true and dequeues the
 * front item, or false if empty.
 *
 * Optionally constrained by element type and maximum capacity.
 */
public class EventValue extends AbstractValue {

    private final ConcurrentLinkedQueue<AbstractValue> mQueue = new ConcurrentLinkedQueue<>();
    private final int mCapacity;        // 0 = unlimited
    private final String mElementType;  // null = any type

    public EventValue() {
        this(0, null);
    }

    public EventValue(int capacity, String elementType) {
        mCapacity = Math.max(0, capacity);
        mElementType = (elementType != null && !elementType.isBlank()) ? elementType.trim() : null;
    }

    public void enqueue(AbstractValue value) {
        mQueue.add(value);
        // If capacity is limited and exceeded, drop oldest
        while (mCapacity > 0 && mQueue.size() > mCapacity) {
            mQueue.poll();
        }
    }

    public AbstractValue dequeue() {
        return mQueue.poll();
    }

    public AbstractValue peek() {
        return mQueue.peek();
    }

    public boolean isEmpty() {
        return mQueue.isEmpty();
    }

    public int size() {
        return mQueue.size();
    }

    public int getCapacity() {
        return mCapacity;
    }

    public String getElementType() {
        return mElementType;
    }

    @Override
    public Type getType() {
        return Type.EVENT;
    }

    @Override
    public String getAbstractSyntax() {
        return "EventValue" + typeDescriptor() + "[" + mQueue.size() + "]";
    }

    @Override
    public String getConcreteSyntax() {
        return "Event" + typeDescriptor() + "[" + mQueue.size() + "]";
    }

    @Override
    public String getFormattedSyntax() {
        return "Event" + typeDescriptor() + "[" + mQueue.size() + "]";
    }

    /**
     * Build the parenthesized type descriptor, e.g. "(String, 10)" or "(String)" or "".
     */
    private String typeDescriptor() {
        if (mElementType != null && mCapacity > 0) {
            return "(" + mElementType + ", " + mCapacity + ")";
        } else if (mElementType != null) {
            return "(" + mElementType + ")";
        } else if (mCapacity > 0) {
            return "(*, " + mCapacity + ")";
        }
        return "";
    }

    @Override
    public Object getValue() {
        return null;
    }

    @Override
    public EventValue getCopy() {
        EventValue copy = new EventValue(mCapacity, mElementType);
        for (AbstractValue v : mQueue) {
            copy.enqueue(v.getCopy());
        }
        return copy;
    }

    @Override
    public boolean equalsValue(AbstractValue value) {
        return value instanceof EventValue;
    }
}
