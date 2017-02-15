//package de.dfki.vsm.model.sceneflow.glue.command.expression.condition;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.model.sceneflow.glue.command.Expression;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.LiteralExpression;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.VariableExpression;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.condition.logical.LogicalCond;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.condition.temporal.TemporalCond;
//import de.dfki.vsm.util.xml.XMLParseError;
//
//import org.w3c.dom.Element;
//
///**
// * An abstract condition.
// *
// * @author Gregor Mehlmann
// */
//public abstract class Condition extends Expression {
//    public enum CondType {
//        CONST, LOGIC, LEXP, EMPTY, TEMP, CONTAINS
//    }
//
//    public abstract CondType getCondType();
//
//    public abstract Condition getCopy();
//
//    public static Condition parse(Element element) throws XMLParseError {
//        Condition cond = null;
//        String    tag  = element.getTagName();
//
//        if (tag.equals("Empty")) {
//            cond = new EmptyCond();
//            cond.parseXML(element);
//        } else if (tag.equals("Contains")) {
//            cond = new ContainsCond();
//            cond.parseXML(element);
//        } else if (tag.equals("Member") || tag.equals("Variable") || tag.equals("Field")) {
//            cond = VariableExpression.parse(element);
//        } else if (tag.equals("Int") || tag.equals("Float") || tag.equals("Object") || tag.equals("Bool")
//                   || tag.equals("String") || tag.equals("List") || tag.equals("Struct")) {
//            cond = LiteralExpression.parse(element);
//        } else if (tag.equals("TimeoutCondition")) {
//            cond = TemporalCond.parse(element);
//        } else {
//            cond = LogicalCond.parse(element);
//        }
//
//        return cond;
//    }
//}
