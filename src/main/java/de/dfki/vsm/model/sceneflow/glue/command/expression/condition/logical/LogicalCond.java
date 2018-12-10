//package de.dfki.vsm.model.sceneflow.glue.command.expression.condition.logical;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.PrologQuery;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.InStateCond;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.HistoryContains;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.condition.Condition;
//import de.dfki.vsm.model.sceneflow.glue.command.expression.condition.Condition.CondType;
//import de.dfki.vsm.util.xml.XMLParseError;
//
//import org.w3c.dom.Element;
//
///**
// * An abstract logical condition
// *
// * @author Gregor Mehlmann
// */
//public abstract class LogicalCond extends Condition {
//    public enum LogicalType {
//        UN, BIN, COMP, DEFAULT, STATE, HC, PROLOG
//    }
//
//   // public abstract LogicalType getLogicalType();
//
//    public abstract LogicalCond getCopy();
//
//    public CondType getCondType() {
//        return CondType.LOGIC;
//    }
//
//    public static LogicalCond parse(Element element) throws XMLParseError {
//        LogicalCond log = null;
//        String      tag = element.getTagName();
//
//        if (tag.equals("And") || tag.equals("Or")) {
//            log = new BinaryCond();
//        } else if (tag.equals("Not")) {
//            log = new UnaryCond();
//        } else if (tag.equals("Eq") || tag.equals("Ge") || tag.equals("Gt") || tag.equals("Le") || tag.equals("Lt")
//                   || tag.equals("Neq")) {
//            log = new ComparisionCond();
//        } else if (tag.equals("DefaultCondition")) {
//            log = new DefaultCond();
//        } else if (tag.equals("StateCondition")) {
//            log = new InStateCond();
//        } else if (tag.equals("HistoryContainsState")) {
//            log = new HistoryContains();
//        } else if (tag.equals("PrologQuery")) {
//            log = new PrologQuery();
//        } else {
//            throw new XMLParseError(null,
//                                    "Cannot parse the element with the tag \"" + tag + "\" into a logical condition!");
//        }
//
//        log.parseXML(element);
//
//        return log;
//    }
//}
