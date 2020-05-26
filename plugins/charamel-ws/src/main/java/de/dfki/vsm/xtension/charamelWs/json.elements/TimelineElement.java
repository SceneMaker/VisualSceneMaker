package de.dfki.vsm.xtension.charamelWs.json.elements;

public class TimelineElement extends JSONElement{
    String uuid,name,track,timestamp,duration,data;
    TlSubtype subtype;

    public TimelineElement( TlSubtype subtype, String uuid, String name, String track, String timestamp, String duration, String data) {
        this.type = JSONElementType.timelineelement;
        this.subtype = subtype;
        this.uuid = uuid;
        this.name = name;
        this.track = track;
        this.timestamp = timestamp;
        this.duration = duration;
        this.data = data;
    }
    /*
    public String toString(){
        StringBuilder json = new StringBuilder();
        json.append("{");
        json.append("{");


                +
                        "      \"type\": \"timeline-element\",\n" +
                        "      \"subtype\": \"tts\",\n" +
                        "      \"uuid\": \"606fee1b-ad84-406d-9667-bd5822291166\",\n" +
                        "      \"name\": \"hello\",\n" +
                        "      \"track\": \"uuid_tts\",\n" +
                        "      \"timestamp\": 200,\n" +
                        "      \"duration\": 5000,\n" +
                        "      \"data\": {\n" +
                        "        \"text\": \"Hello,my name is Gloria\"\n" +
                        "      }\n" +
                        "    },\n" +

)
    }

     */
}
