package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DecadExecutorTest {

    private DecadExecutor executor;


    @Test
    void shouldReturnActivityExecutorInstance() {
        DecadExecutor executor = makeDefaultExecutor();
        assertTrue(executor instanceof ActivityExecutor);
    }

    @Test
    void shouldReturnNewMarker() {
        executor = makeDefaultExecutor();
        String markerResult = executor.marker(3);
        assertEquals("#3#", markerResult);

    }


    @NotNull
    private DecadExecutor makeDefaultExecutor() {
        return new DecadExecutor(new PluginConfig(), new RunTimeProject());
    }
}