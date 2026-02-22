package de.dfki.vsm.runtime.interpreter.value;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class EventValueTest {

    @Test
    void boundedQueueDropsOldestWhenCapacityExceeded() {
        EventValue eventValue = new EventValue(3, null);

        eventValue.enqueue(new StringValue("a"));
        eventValue.enqueue(new StringValue("b"));
        eventValue.enqueue(new StringValue("c"));
        eventValue.enqueue(new StringValue("d"));

        assertEquals(3, eventValue.size(), "Queue must stay at configured capacity");
        assertEquals("b", ((StringValue) eventValue.dequeue()).getValue(), "Oldest value should be dropped");
        assertEquals("c", ((StringValue) eventValue.dequeue()).getValue());
        assertEquals("d", ((StringValue) eventValue.dequeue()).getValue());
        assertTrue(eventValue.isEmpty(), "Queue should be empty after consuming all elements");
    }

    @Test
    void wildcardEventTypeAcceptsMixedValueTypes() {
        EventValue eventValue = new EventValue(10, null); // Equivalent to Event(*, 10)

        eventValue.enqueue(new IntValue(7));
        eventValue.enqueue(new StringValue("x"));
        eventValue.enqueue(new BooleanValue(true));
        eventValue.enqueue(new FloatValue(1.5f));

        assertEquals(4, eventValue.size());
        assertTrue(eventValue.dequeue() instanceof IntValue);
        assertTrue(eventValue.dequeue() instanceof StringValue);
        assertTrue(eventValue.dequeue() instanceof BooleanValue);
        assertTrue(eventValue.dequeue() instanceof FloatValue);
    }

    @Test
    void zeroCapacityMeansUnboundedQueue() {
        EventValue eventValue = new EventValue(0, null); // 0 = unlimited

        for (int i = 0; i < 50; i++) {
            eventValue.enqueue(new IntValue(i));
        }

        assertEquals(50, eventValue.size(), "Unlimited queue should not evict elements");
        assertEquals(0, ((IntValue) eventValue.dequeue()).intValue());
        assertEquals(1, ((IntValue) eventValue.dequeue()).intValue());
    }
}
