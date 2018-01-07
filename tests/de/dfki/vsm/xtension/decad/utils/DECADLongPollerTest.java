package de.dfki.vsm.xtension.decad.utils;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import fakes.FakeDecadExecutor;
import fakes.FakeIsSpeakingCommand;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertTrue;


class DECADLongPollerTest {

    private DECADLongPoller poller;

    @Test
    void shouldExecuteExecutorOnTimeOut() {
        final FakeDecadExecutor fakeExecutor = createPoller();
        poller.startPolling();
        assertTrue(fakeExecutor.isBlocked);
    }

    @NotNull
    private FakeDecadExecutor createPoller() {
        final FakeDecadExecutor fakeExecutor = new FakeDecadExecutor(new PluginConfig(), new RunTimeProject());
        poller = new DECADLongPoller(fakeExecutor, new FakeIsSpeakingCommand(null));
        return fakeExecutor;
    }


}