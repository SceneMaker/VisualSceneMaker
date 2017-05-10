package de.dfki.vsm.xtension.remotesender.server.factories;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remotesender.server.parsers.Parser;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.DummyParser;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.UIResponseParser;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

/**
 * Created by alvaro on 5/10/17.
 */
public class VariableSetterParser extends ParserFactory {

    private final RunTimeProject project;

    public VariableSetterParser(RunTimeProject project){
        this.project = project;
    }
    @Override
    public Parser buildParser(String type) {
        switch (type){
            case "uiresponse":
                return getUiResponseParser();
            default:
                return new DummyParser(data);
        }
    }

    private Parser getUiResponseParser() {
        try {
            return new UIResponseParser(data, project);
        } catch (IOException | SAXException | ParserConfigurationException e) {
            return new DummyParser(data);
        }
    }
}
