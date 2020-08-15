package de.dfki.vsm.xtension.charamelWs;

import java.util.UUID;

class Strings {
    static String testMsg = "{\n" +
            "  \"type\": \"animation\",\n" +
            "  \"name\": \"\",\n" +
            "  \"uuid\": \"58a11974-146c-4e3a-ab47-b180922cdec9\",\n" +
            "  \"timeline\": [\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"tts\",\n" +
            "      \"uuid\": \"606fee1b-ad84-406d-9667-bd5822291166\",\n" +
            "      \"name\": \"hello\",\n" +
            "      \"track\": \"uuid_tts\",\n" +
            "      \"timestamp\": 200,\n" +
            "      \"duration\": 5000,\n" +
            "      \"data\": {\n" +
            "        \"text\": \"Hello,my name is Gloria\",\n" +
            "        \"voice\": \"Joanna\"\n" +
            "      }\n" +
            "    },\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"motion\",\n" +
            "      \"uuid\": \"d291dacd-d53d-4968-b425-cfaa9e44a6cc\",\n" +
            "      \"name\": \"greet01.glb\",\n" +
            "      \"track\": \"motions1_uuid\",\n" +
            "      \"timestamp\": 0,\n" +
            "      \"duration\": 2400,\n" +
            "      \"data\": {\n" +
            "        \"attack\": 500,\n" +
            "        \"decay\": 500,\n" +
            "        \"speed\": 1,\n" +
            "        \"path\": \"humanoid/interaction/greet/greet01.glb\"\n" +
            "      }\n" +
            "    },\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"morph\",\n" +
            "      \"uuid\": \"10536d63-fc88-4183-aed5-735ea9170da2\",\n" +
            "      \"name\": \"emot_happy\",\n" +
            "      \"track\": \"morph_uuid\",\n" +
            "      \"timestamp\": 600,\n" +
            "      \"duration\": 2000,\n" +
            "      \"data\": {\n" +
            "        \"morph\": \"uuid_emot_happy\",\n" +
            "        \"attack\": 500,\n" +
            "        \"decay\": 500\n" +
            "      }\n" +
            "    }\n" +
            "  ]\n" +
            "}";
    static String launchString = "{\n" +
            "  \"type\": \"animation\",\n" +
            "  \"name\": \"\",\n" +
            "  \"uuid\": \"58a11974-146c-4e3a-ab47-b180922cdec9\",\n" +
            "  \"timeline\": [\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"tts\",\n" +
            "      \"uuid\": \"606fee1b-ad84-406d-9667-bd5822291166\",\n" +
            "      \"name\": \"hello\",\n" +
            "      \"track\": \"uuid_tts\",\n" +
            "      \"timestamp\": 200,\n" +
            "      \"duration\": 5000,\n" +
            "      \"data\": {\n" +
            "        \"text\": \"Hello,my name is Gloria\",\n" +
            "        \"voice\": \"Joanna\"\n" +
            "      }\n" +
            "    },\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"motion\",\n" +
            "      \"uuid\": \"d291dacd-d53d-4968-b425-cfaa9e44a6cc\",\n" +
            "      \"name\": \"greet01.glb\",\n" +
            "      \"track\": \"motions1_uuid\",\n" +
            "      \"timestamp\": 0,\n" +
            "      \"duration\": 2400,\n" +
            "      \"data\": {\n" +
            "        \"attack\": 500,\n" +
            "        \"decay\": 500,\n" +
            "        \"speed\": 1,\n" +
            "        \"path\": \"humanoid/interaction/greet/greet01.glb\"\n" +
            "      }\n" +
            "    },\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"morph\",\n" +
            "      \"uuid\": \"10536d63-fc88-4183-aed5-735ea9170da2\",\n" +
            "      \"name\": \"emot_happy\",\n" +
            "      \"track\": \"morph_uuid\",\n" +
            "      \"timestamp\": 600,\n" +
            "      \"duration\": 2000,\n" +
            "      \"data\": {\n" +
            "        \"morph\": \"uuid_emot_happy\",\n" +
            "        \"attack\": 500,\n" +
            "        \"decay\": 500\n" +
            "      }\n" +
            "    }\n" +
            "  ]\n" +
            "}";

    static String speakCommand(String voice, String text, String actorId) {
        return ("{\n" +
                "  \"type\": \"animation\",\n" +
                "  \"name\": \"\",\n" +
                "  \"uuid\": \"" + UUID.randomUUID() + "\",\n" +
                "  \"timeline\": [\n" +
                new SpeakCommand(text, voice, actorId).toJsonCommand() +
                "  ]\n" +
                "}");
    }

    static String waveCommand = "{\n" +
            "  \"type\": \"animation\",\n" +
            "  \"name\": \"\",\n" +
            "  \"uuid\": \"58a11974-146c-4e3a-ab47-b180922cdec9\",\n" +
            "  \"timeline\": [\n" +
            "    {\n" +
            "      \"type\": \"timeline-element\",\n" +
            "      \"subtype\": \"motion\",\n" +
            "      \"uuid\": \"d291dacd-d53d-4968-b425-cfaa9e44a6cc\",\n" +
            "      \"name\": \"greet01.glb\",\n" +
            "      \"track\": \"motions1_uuid\",\n" +
            "      \"timestamp\": 0,\n" +
            "      \"duration\": 2400,\n" +
            "      \"data\": {\n" +
            "        \"attack\": 500,\n" +
            "        \"decay\": 500,\n" +
            "        \"speed\": 1,\n" +
            "        \"path\": \"humanoid/interaction/greet/greet01.glb\"\n" +
            "      }\n" +
            "    }\n" +
            "  ]\n" +
            "}";
}
