package de.dfki.vsm.xtension.smartjacket;

import java.util.List;

public class BTCmd {
    String service;
    List<String> characteristics;
    String descriptor;
    String properties;
    String input;
    String id;

    public BTCmd(String service, List<String> characteristics, String descriptor, String properties, String input, String id) {
        this.service = service;
        this.characteristics = characteristics;
        this.descriptor = descriptor;
        this.properties = properties;
        this.input = input;
        this.id = id;
    }

    public String getService() {
        return service;
    }

    public void setService(String service) {
        this.service = service;
    }

    public List<String> getCharacteristics() {
        return characteristics;
    }

    public void setCharacteristics(List<String> characteristics) {
        this.characteristics = characteristics;
    }

    public String getDescriptor() {
        return descriptor;
    }

    public void setDescriptor(String descriptor) {
        this.descriptor = descriptor;
    }

    public String getProperties() {
        return properties;
    }

    public void setProperties(String properties) {
        this.properties = properties;
    }

    public String getInput() {
        return input;
    }

    public void setInput(String input) {
        this.input = input;
    }
}
