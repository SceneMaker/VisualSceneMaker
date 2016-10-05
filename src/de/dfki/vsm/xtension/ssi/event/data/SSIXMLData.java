//package de.dfki.vsm.xtension.ssi.event.data;
//
//import de.dfki.vsm.util.ios.IOSIndentWriter;
//import java.io.ByteArrayOutputStream;
//
///**
// * @author Gregor Mehlmann
// */
//public class SSIXMLData implements XMLParseable, XMLWriteable {
//
//    // The XML string data
//    final String mData;
//
//    // Construct XML data
//    public SSIXMLData(final String data) {
//        mData = data;
//    }
//
//    public final String getXML() {
//        return mData;
//    }
//
//    // Get string representation
//    @Override
//    public final String toString() {
//        final ByteArrayOutputStream stream = new ByteArrayOutputStream();
//        final IOSIndentWriter writer = new IOSIndentWriter(stream);
//        writer.print("<![CDATA[" + mData + "]]>");
//        writer.flush();
//        writer.close();
//        try {
//            return stream.toString("UTF-8");
//        } catch (final Exception exc) {
//            return stream.toString();
//        }
//    }
//}
