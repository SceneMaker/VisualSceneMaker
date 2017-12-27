package de.dfki.vsm.xtension.decad.commands;

import de.dfki.vsm.runtime.activity.ActionActivity;
import fakes.FakeHttpClient;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedList;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AnimationCommandTest {
    @Test
    void shouldExecuteAnimationGETRequest() {
        AnimationCommand command = new AnimationCommand(new ActionActivity("test", "Waving", "Wave",
                new LinkedList<>(), new HashMap<>()));
        FakeHttpClient client = new FakeHttpClient();
        command.setHttpClient(client);
        command.execute();
        assertEquals("http://localhost:5005/animation/animate/Waving", client.getUrl());
    }


}