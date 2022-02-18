import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.runtime.activity.ActionActivity;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.charamelWs.Charamel;
import de.dfki.vsm.xtension.charamelWs.Commands.EmotionCommand;
import de.dfki.vsm.xtension.util.mocks.TestComm;
import de.dfki.vsm.xtension.util.mocks.TestRuntime;
import de.dfki.vsm.xtension.util.plugin.DrivenPlugin;
import de.dfki.vsm.xtension.util.runtime.DrivenRuntime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.LinkedList;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CharamelTest {
    private DrivenPlugin charamel;
    private TestComm comm;
    private DrivenRuntime runtime;

    @BeforeEach
    void setUp() {
        this.comm = new TestComm();
        this.runtime = new TestRuntime();
        this.charamel = new Charamel(comm,
                "var",
                LOGConsoleLogger.getInstance(),
                runtime);
    }

    @Test
    void angrytoJson() {
        assertEquals("    {\n" +
                        "      \"type\": \"emotion\",\n" +
                        "      \"name\": \"emot_angry\",\n" +
                        "      \"attack\": 200,\n" +
                        "      \"hold\": 1000,\n" +
                        "      \"decay\": 200,\n" +
                        "      \"value\": 1.0\n" +
                        "    }\n",
                new EmotionCommand("emot_angry", 1).toJson());
    }

    @Test
    void angry() {
        LinkedList<ActionFeature> values = new LinkedList<>();
        values.add(new ActionFeature(0, 0, "intensity", "1.0"));
        HashMap<String, String> subs = new HashMap<>();
        charamel.execute(new ActionActivity("test", "angry", "", values, subs));
        assertEquals(new EmotionCommand("emot_angry", 1).toJson(), comm.messages.get(0));
    }

    @Test
    void marker() {
        assertEquals("${'1'}$", charamel.marker(1));
    }
}