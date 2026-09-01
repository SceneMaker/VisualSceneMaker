package de.dfki.vsm.xtension.alma;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** Exercises AlmaActRules against the plugin's own bundled default project XML. */
class AlmaActRulesTest {

    private static final Path DEFAULT_PROJECT_XML = repoRoot()
            .resolve("plugins/alma/src/main/resources/templates/alma/default-project.xml");

    private static Path repoRoot() {
        Path candidate = Path.of("").toAbsolutePath();
        for (int i = 0; i < 4 && candidate != null; i++) {
            if (Files.isDirectory(candidate.resolve("plugins/alma"))) {
                return candidate;
            }
            candidate = candidate.getParent();
        }
        throw new IllegalStateException("Could not locate repository root from " + Path.of("").toAbsolutePath());
    }

    private static AlmaActRules loadDefaultProjectRules() throws IOException {
        return AlmaActRules.parse(Files.readString(DEFAULT_PROJECT_XML));
    }

    @Test
    void resolvesSelfActToItsUnderlyingTags() throws IOException {
        AlmaActRules rules = loadDefaultProjectRules();

        // <SelfAct type="Excuse"><BadEvent .../><BadActSelf .../></SelfAct> under Anne.
        List<String> tags = rules.resolve("Anne", "Excuse", null);

        assertEquals(List.of("BadEvent", "BadActSelf"), tags);
    }

    @Test
    void resolvesDirectActByPerformer() throws IOException {
        AlmaActRules rules = loadDefaultProjectRules();

        // <DirectAct type="PraiseAction" performer="Bruno"><GoodActSelf .../></DirectAct> under Anne.
        List<String> tags = rules.resolve("Anne", "PraiseAction", "Bruno");

        assertEquals(List.of("GoodActSelf"), tags);
    }

    @Test
    void differentPerformersResolveDifferentRulesForTheSameAct() throws IOException {
        AlmaActRules rules = loadDefaultProjectRules();

        // Anne has DirectAct type="AnnounceConcern" for both Anne and Bruno, with different tags.
        List<String> fromAnne = rules.resolve("Anne", "AnnounceConcern", "Anne");
        List<String> fromBruno = rules.resolve("Anne", "AnnounceConcern", "Bruno");

        assertEquals(List.of("BadLikelyFutureEvent"), fromAnne);
        assertEquals(List.of("BadActOther"), fromBruno);
    }

    @Test
    void resolvesIndirectActByPerformerWhenNoDirectActMatches() throws IOException {
        AlmaActRules rules = loadDefaultProjectRules();

        // Bruno has <IndirectAct type="Insult" performer="Clementine"> and no matching DirectAct
        // (Bruno's only DirectAct type="Insult" is for a different performer, "Anne").
        List<String> tags = rules.resolve("Bruno", "Insult", "Clementine");

        assertEquals(List.of("BadEvent", "BadActOther"), tags);
    }

    @Test
    void unknownActOrCharacterResolvesToEmpty() throws IOException {
        AlmaActRules rules = loadDefaultProjectRules();

        assertTrue(rules.resolve("Anne", "NoSuchAct", null).isEmpty());
        assertTrue(rules.resolve("NoSuchCharacter", "Excuse", null).isEmpty());
        assertTrue(rules.resolve("Anne", "PraiseAction", "NoSuchPerformer").isEmpty());
    }

    @Test
    void blankAndNullXmlYieldEmptyRuleSet() {
        AlmaActRules rules = AlmaActRules.parse(null);
        assertTrue(rules.resolve("Anne", "Excuse", null).isEmpty());

        AlmaActRules blank = AlmaActRules.parse("   ");
        assertTrue(blank.resolve("Anne", "Excuse", null).isEmpty());
    }
}
