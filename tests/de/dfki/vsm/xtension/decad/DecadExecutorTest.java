package de.dfki.vsm.xtension.decad;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import fakes.FakeCommand;
import fakes.FakeDecadExecutor;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;

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
        assertEquals("$3", markerResult);
    }

    @Test
    void shouldExecuteCommand() {
        executor = makeDefaultExecutor();
        executor.execute(new SpeechActivity("Test", new LinkedList(), "."));
        FakeCommand command = (FakeCommand) ((FakeDecadExecutor) executor).getExecutedCommand();
        assertTrue(command.executed);

    }

    @NotNull
    private DecadExecutor makeDefaultExecutor() {
        return new FakeDecadExecutor(new PluginConfig(), new RunTimeProject());
    }
}