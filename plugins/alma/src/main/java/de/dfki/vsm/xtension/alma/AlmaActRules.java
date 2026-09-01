package de.dfki.vsm.xtension.alma;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilderFactory;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Resolves a "dialogue act" (e.g. "Accuse", "Insult") authored as a SceneFlow action into the
 * underlying basic appraisal tag(s) the ALMA server actually understands, per character, using each
 * character's own SelfAct/DirectAct/IndirectAct rules from the project's AffectDefinition XML.
 *
 * <p>The server has no wire message for "run act X" — VSM never sends act names over the wire, only
 * the same tag+intensity+elicitor form basic appraisal tags already use
 * ({@code AlmaWsClient#sendAppraisal}). Resolving the act to its tags client-side, from the same XML
 * already uploaded to the server, keeps the two in lockstep without inventing an undocumented
 * server-side extension.
 */
final class AlmaActRules {

    /** DirectAct/IndirectAct rules are looked up by (type, performer); SelfAct needs no performer. */
    private record ActRuleKey(String type, String performer) {
    }

    private final Map<String, Map<String, List<String>>> mSelfActs = new HashMap<>();
    private final Map<String, Map<ActRuleKey, List<String>>> mDirectActs = new HashMap<>();
    private final Map<String, Map<ActRuleKey, List<String>>> mIndirectActs = new HashMap<>();

    private AlmaActRules() {
    }

    static AlmaActRules parse(final String xml) {
        AlmaActRules rules = new AlmaActRules();
        if (xml == null || xml.isBlank()) {
            return rules;
        }
        try {
            Document doc = DocumentBuilderFactory.newInstance().newDocumentBuilder()
                    .parse(new InputSource(new StringReader(xml)));
            NodeList characters = doc.getElementsByTagName("CharacterAffect");
            for (int i = 0; i < characters.getLength(); i++) {
                Element characterEl = (Element) characters.item(i);
                String character = characterEl.getAttribute("name").trim();
                if (character.isEmpty()) {
                    continue;
                }
                Map<String, List<String>> selfActs = new HashMap<>();
                Map<ActRuleKey, List<String>> directActs = new HashMap<>();
                Map<ActRuleKey, List<String>> indirectActs = new HashMap<>();

                for (Element actEl : childElements(characterEl, "SelfAct")) {
                    String type = actEl.getAttribute("type").trim();
                    if (!type.isEmpty()) {
                        selfActs.put(type, tagNames(actEl));
                    }
                }
                for (Element actEl : childElements(characterEl, "DirectAct")) {
                    String type = actEl.getAttribute("type").trim();
                    String performer = actEl.getAttribute("performer").trim();
                    if (!type.isEmpty() && !performer.isEmpty()) {
                        directActs.put(new ActRuleKey(type, performer), tagNames(actEl));
                    }
                }
                for (Element actEl : childElements(characterEl, "IndirectAct")) {
                    String type = actEl.getAttribute("type").trim();
                    String performer = actEl.getAttribute("performer").trim();
                    if (!type.isEmpty() && !performer.isEmpty()) {
                        indirectActs.put(new ActRuleKey(type, performer), tagNames(actEl));
                    }
                }

                rules.mSelfActs.put(character, selfActs);
                rules.mDirectActs.put(character, directActs);
                rules.mIndirectActs.put(character, indirectActs);
            }
        } catch (Exception ignore) {
            // Caller logs nothing here on purpose; an empty rule set just means acts won't
            // resolve for anyone, same as an author invoking an unrecognized action name today.
        }
        return rules;
    }

    /**
     * @param performer blank/null resolves a SelfAct (the character's own act); otherwise tries a
     *                  DirectAct from that performer, then an IndirectAct from that performer.
     * @return the appraisal tag(s) to send, or an empty list if no rule matches.
     */
    List<String> resolve(final String character, final String actType, final String performer) {
        if (character == null || actType == null) {
            return List.of();
        }
        if (performer == null || performer.isBlank()) {
            return mSelfActs.getOrDefault(character, Map.of()).getOrDefault(actType, List.of());
        }
        ActRuleKey key = new ActRuleKey(actType, performer);
        List<String> direct = mDirectActs.getOrDefault(character, Map.of()).get(key);
        if (direct != null) {
            return direct;
        }
        return mIndirectActs.getOrDefault(character, Map.of()).getOrDefault(key, List.of());
    }

    private static List<Element> childElements(final Element parent, final String tagName) {
        List<Element> result = new ArrayList<>();
        NodeList children = parent.getElementsByTagName(tagName);
        for (int i = 0; i < children.getLength(); i++) {
            result.add((Element) children.item(i));
        }
        return result;
    }

    private static List<String> tagNames(final Element actEl) {
        List<String> tags = new ArrayList<>();
        NodeList children = actEl.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            if (node instanceof Element el) {
                tags.add(el.getTagName());
            }
        }
        return tags;
    }
}
