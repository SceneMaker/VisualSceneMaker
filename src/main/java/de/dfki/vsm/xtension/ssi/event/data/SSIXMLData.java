//package de.dfki.vsm.xtension.ssi.event.data;
//
//import de.dfki.vsm.util.log.LOGConsoleLogger;
//import java.io.StringWriter;
//import javax.xml.transform.Transformer;
//import javax.xml.transform.TransformerException;
//import javax.xml.transform.TransformerFactory;
//import javax.xml.transform.dom.DOMSource;
//import javax.xml.transform.stream.StreamResult;
//import org.w3c.dom.Element;
//
///**
// * @author Gregor Mehlmann
// */
//public final class SSIXMLData extends SSIEventData {
//
//    // The singelton logger instance
//    private final LOGConsoleLogger mLogger
//            = LOGConsoleLogger.getInstance();
//    // The XML DOM element data
//    private final Element mElement;
//
//    // Construct XML data
//    public SSIXMLData(final Element element) {
//        mElement = element;
//    }
//
//    public final Element getXMLElement() {
//        return mElement;
//    }
//
//    // Get string representation
//    @Override
//    public final String toString() {
//        try {
//            final Transformer transformer
//                    = TransformerFactory.newInstance().newTransformer();
//            final StreamResult result
//                    = new StreamResult(new StringWriter());
//            final DOMSource source = new DOMSource(mElement);
//            transformer.transform(source, result);
//            return result.getWriter().toString();
//        } catch (final TransformerException exc) {
//            return null;
//        }
//    }
//}
