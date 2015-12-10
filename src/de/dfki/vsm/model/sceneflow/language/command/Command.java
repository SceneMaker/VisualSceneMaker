package de.dfki.vsm.model.sceneflow.language.command;

import de.dfki.vsm.model.sceneflow.language.SyntaxObject;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public abstract class Command implements SyntaxObject {

    public static Command parse(Element element) throws XMLParseError {
        // The expression to parse
        Command command;
        // The name of the XML tag
        final String tag = element.getTagName();
        // Check the commmand 
        if (tag.equals("Assignment")) {
            command = new Assignment();
            command.parseXML(element);
        } else {
            // Try to parse expression
            command = Expression.parse(element);
            if (command == null) {
                // Try to parse invocation
                command = Invocation.parse(element);
            }
        }
        return command;
    }

    @Override
    public abstract Command getCopy();
}
