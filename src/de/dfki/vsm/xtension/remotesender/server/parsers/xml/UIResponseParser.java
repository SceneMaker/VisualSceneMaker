package de.dfki.vsm.xtension.remotesender.server.parsers.xml;

import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtension.remotesender.commands.UIResponseNotification;
import de.dfki.vsm.xtension.remotesender.server.notifications.NotifiableCommand;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.exceptions.InvalidValue;
import de.dfki.vsm.xtension.remotesender.server.parsers.xml.exceptions.NoValueProvided;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;


/**
 * Created by alvaro on 4/28/17.
 */
public class UIResponseParser extends ResponseParser {

    public UIResponseParser(String data, RunTimeProject project, String variableName) throws IOException, SAXException, ParserConfigurationException {
        super(data);
        notification = new UIResponseNotification(project, variableName);
    }

    @Override
    public NotifiableCommand parse() throws InvalidValue, NoValueProvided {
        getAllValues();
        String strValue = values.get(0);
        getAsUIResponse().setSelectedOption(strValue);
        return notification;
    }

    private UIResponseNotification getAsUIResponse() {
        return (UIResponseNotification)notification;
    }

}
