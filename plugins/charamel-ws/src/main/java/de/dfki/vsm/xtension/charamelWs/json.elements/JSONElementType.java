package de.dfki.vsm.xtension.charamelWs.json.elements;

public enum JSONElementType {
    animation("animations"),timelineelement("timeline-element");

    private String value;
    private JSONElementType(String value)
    {
        this.value = value;
    }

    public String toString()
    {
        return this.value;
    }

}
