package de.dfki.vsm.model.sceneflow.graphics.comment;

import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;

/**
 * @author Gregor Mehlmann
 */
public final class CommentGraphics implements ModelObject {

    private CommentBoundary mRectangle;

    public CommentGraphics() {
        mRectangle = new CommentBoundary();
    }

    public CommentGraphics(final CommentBoundary rectangle) {
        mRectangle = rectangle;
    }

    public final void setRectangle(final CommentBoundary value) {
        mRectangle = value;
    }

    public final CommentBoundary getRectangle() {
        return mRectangle;
    }

    @Override
    public final CommentGraphics getCopy() {
        return new CommentGraphics(mRectangle.getCopy());
    }

    @Override
    public final void writeXML(final IOSIndentWriter out) {
        out.println("<Graphics>").push();
        mRectangle.writeXML(out);
        out.pop().println("</Graphics>");
    }

    @Override
    public final void parseXML(final Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, "Boundary", new XMLParseAction() {
            @Override
            public void run(final Element element) throws XMLParseError {
                mRectangle.parseXML(element);
            }
        });
    }
}
