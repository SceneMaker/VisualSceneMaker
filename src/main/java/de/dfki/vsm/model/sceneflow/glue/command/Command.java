package de.dfki.vsm.model.sceneflow.glue.command;

import de.dfki.vsm.model.sceneflow.glue.SyntaxObject;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Command extends SyntaxObject {

    @Override
    public abstract Command getCopy();

    public static Command parse(final Element element) throws XMLParseError {
        // The command to parse
        Command command = null;
        // The name of the XML tag
        final String tag = element.getTagName();
        // Parse the command
        if (tag.equals("Assignment")) {
            command = new Assignment();
            command.parseXML(element);
        } else {
            command = Invocation.parse(element);
        }
        // Parse the expression
        if (command == null) {
            command = Expression.parse(element);
        }
        return command;
    }
}
