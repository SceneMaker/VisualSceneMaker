package de.dfki.vsm.xtension.remotesender.server.parsers.xml;


import de.dfki.vsm.xtension.remotesender.server.notifications.NotifiableCommand;
import de.dfki.vsm.xtension.remotesender.server.parsers.Parser;

/**
 * Created by alvaro on 4/30/17.
 */
public class DummyParser implements Parser {
    public DummyParser(String data) {
    }

    @Override
    public NotifiableCommand parse() {
        return new NotifiableCommand() {
            @Override
            public void execute() {

            }

            @Override
            public void setValue(Object value) {

            }
        };
    }
}
