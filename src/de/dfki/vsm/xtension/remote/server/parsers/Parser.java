package de.dfki.vsm.xtension.remote.server.parsers;


import de.dfki.vsm.xtension.remote.server.commands.NotifiableCommand;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.InvalidValue;
import de.dfki.vsm.xtension.remote.server.parsers.xml.exceptions.NoValueProvided;

/**
 * Created by alvaro on 4/28/17.
 */
public interface Parser {
    NotifiableCommand parse() throws InvalidValue, NoValueProvided;
}
