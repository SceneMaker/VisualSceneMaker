package de.dfki.vsm.xtension.decad.factories;

import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.runtime.activity.SpeechActivity;
import de.dfki.vsm.xtension.decad.commands.AnimationCommand;
import de.dfki.vsm.xtension.decad.commands.DecadCommand;
import de.dfki.vsm.xtension.decad.commands.DummyCommand;
import de.dfki.vsm.xtension.decad.commands.SpeechCommand;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedList;

import static org.junit.jupiter.api.Assertions.assertTrue;

class DecadCommandFactoryTest {
    private DecadCommandFactory factory;

    @Test
    void shouldReturnSpeechCommandWhenSpeechActivity() {
        factory = makeFactory();
        AbstractActivity activity = new SpeechActivity("Test actor", new LinkedList(), ".");
        DecadCommand resultCommand = factory.getCommand(activity);
        assertTrue(resultCommand instanceof SpeechCommand);
    }

    private DecadCommandFactory makeFactory() {
        return new DecadCommandFactory();
    }

    @Test
    void shouldReturnAnimationCommandWhenSpeechActivity() {
        factory = makeFactory();
        AbstractActivity activity = new ActionActivity("Test actor", "Test name", "Test text",
                new LinkedList(), new HashMap<>());
        DecadCommand resultCommand = factory.getCommand(activity);
        assertTrue(resultCommand instanceof AnimationCommand);
    }

    @Test
    void shouldReturnDummyCommandWhenUnknownActivity() {
        factory = makeFactory();
        DecadCommand resultCommand = factory.getCommand(null);
        assertTrue(resultCommand instanceof DummyCommand);
    }

}