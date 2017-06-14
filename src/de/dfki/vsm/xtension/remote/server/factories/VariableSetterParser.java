package de.dfki.vsm.xtension.remote.server.factories;

import de.dfki.vsm.runtime.project.RunTimeProject;

import de.dfki.vsm.xtension.remote.server.parsers.Parser;
import de.dfki.vsm.xtension.remote.server.parsers.DummyParser;
import de.dfki.vsm.xtension.remote.server.parsers.xml.UIResponseParser;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

/**
 * Created by alvaro on 5/10/17.
 */
public class VariableSetterParser extends ParserFactory {

    private final RunTimeProject project;
    private final String variableName;

    public VariableSetterParser(RunTimeProject project, String variableName)
    {
        this.variableName = variableName;
        this.project = project;
    }
    @Override
    public Parser buildParser(String type) {
        switch (type){
            case "uiresponse":
                return getUiResponseParser();
            default:
                return new DummyParser(data, project, variableName);
        }
    }

    private Parser getUiResponseParser() {
        try {
            return new UIResponseParser(data, project, variableName);
        } catch (IOException | SAXException | ParserConfigurationException e) {
            return new DummyParser(data, project, variableName);
        }
    }
}
