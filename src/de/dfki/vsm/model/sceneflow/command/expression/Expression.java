package de.dfki.vsm.model.sceneflow.command.expression;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.Command.CmdType;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

/**
 * @author Not me
 */
public abstract class Expression extends Command {

    // TODO: Rename the expression types
    public enum ExpType {
        BIN, UN, IF, COND, HVO, USR, VO, CONS
    }

    public abstract ExpType getExpType();

    public abstract Expression getCopy();

    public CmdType getCmdType() {
        return CmdType.EXP;
    }

    public static Expression parse(Element element) throws XMLParseError {
        Expression exp = null;
        String     tag = element.getTagName();

        if (tag.equals("UserCommand")) {
            exp = new UsrCmd();
            exp.parseXML(element);
        } else if (tag.equals("If")) {
            exp = new ConditionalExp();
            exp.parseXML(element);
        } else if (tag.equals("Constructor")) {
            exp = new Constructor();
            exp.parseXML(element);
        } else if (tag.equals("Random") || tag.equals("First") || tag.equals("Last") || tag.equals("Clear")
                   || tag.equals("Size") || tag.equals("RemoveFirst") || tag.equals("RemoveLast")
                   || tag.equals("Neg")) {
            exp = new UnaryExp();
            exp.parseXML(element);
        } else if (tag.equals("Add") || tag.equals("Div") || tag.equals("Mul") || tag.equals("Sub")
                   || tag.equals("Get") || tag.equals("Remove") || tag.equals("AddFirst") || tag.equals("AddLast")) {
            exp = new BinaryExp();
            exp.parseXML(element);
        } else if (tag.equals("HistoryValueOf")) {
            exp = new HistoryValueOf();
            exp.parseXML(element);
        } else if (tag.equals("HistoryRunTimeOf")) {
            exp = new HistoryRunTimeOf();
            exp.parseXML(element);
        } else if (tag.equals("ValueOf")) {
            exp = new ValueOf();
            exp.parseXML(element);
        } else {
            exp = Condition.parse(element);
        }

        return exp;
    }
}
