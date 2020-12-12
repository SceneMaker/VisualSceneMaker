package de.dfki.vsm.xtension.odp.id_generator;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public final class TtsIdGen {
    private final AtomicBoolean taskStarted = new AtomicBoolean(false);
    private final Gen idGenerator = new Gen();

    public Generated<Integer> newTaskArrived(String task) {
        if(task.equalsIgnoreCase("cancel")) {
            return idGenerator;
        }
        taskStarted.set(true);
        idGenerator.increment();
        return idGenerator;
    }

    public Generated<Integer> ttsStarted() {
        if (taskStarted.compareAndSet(true, false)) {
            return idGenerator;
        } else {
            idGenerator.increment();
        }
        return idGenerator;
    }

    public Generated<Integer> ttsEnded() {
        return idGenerator;
    }


    private static class Gen implements Generated<Integer> {
        private final AtomicInteger currentId = new AtomicInteger(0);


        public void increment() {
            currentId.incrementAndGet();
        }

        @Override
        public Integer get() {
            return currentId.get();
        }
    }
}
