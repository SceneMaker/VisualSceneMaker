package de.dfki.vsm.model.sceneflow.diagram.boards;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.diagram.SuperNode;
import de.dfki.vsm.model.sceneflow.diagram.graphics.comment.CommentGraphics;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;
import org.w3c.dom.Element;
import java.awt.Font;
import javax.swing.JEditorPane;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

/**
 * @author Gregor Mehlmann
 */
public final class CommentBoard implements /*SyntaxObject*/ ModelObject {

    private SuperNode mParentNode = null;
    private String mHTMLText = "";
    private CommentGraphics mGraphics;
    private int mFontSize;
    private JEditorPane mTextEditor;

    public void setParentNode(SuperNode value) {
        mParentNode = value;
    }

    public CommentGraphics getGraphics() {
        return mGraphics;
    }

    public void setGraphics(CommentGraphics value) {
        mGraphics = value;
    }

    public void setFontSize(int value) {
        mFontSize = value;
    }

    public String getHTMLText() {
        return mHTMLText.trim();
    }

    public void setHTMLText(String text) {
        mHTMLText = text.trim();
    }

//    @Override
//    public String getAbstractSyntax() {
//        return "";
//    }
//
//    @Override
//    public String getConcreteSyntax() {
//        return "";
//    }
//
//    @Override
//    public String getFormattedSyntax() {
//        return "";
//    }

    private void formatHTML() {
        mFontSize
                = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sWORKSPACEFONTSIZE;

        if (mTextEditor == null) {
            mTextEditor = new JEditorPane();
        }

        mTextEditor.setContentType(new HTMLEditorKit().getContentType());

        // now use the same font than the label!
        Font mFont = new Font("SansSerif", Font.PLAIN, mFontSize);
        String bodyRule = "body { font-family: " + mFont.getFamily() + "; " + "font-size: " + mFont.getSize() + "pt; }";

        ((HTMLDocument) mTextEditor.getDocument()).getStyleSheet().addRule(bodyRule);
        mTextEditor.setText(mHTMLText);
        mHTMLText = mTextEditor.getText();
    }

    @Override
    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
            @Override
            public void run(Element element) throws XMLParseError {
                String tag = element.getTagName();

                if (tag.equals("Graphics")) {
                    mGraphics = new CommentGraphics();
                    mGraphics.parseXML(element);
                } else if (tag.equals("Text")) {
                    mHTMLText = element.getTextContent();
                } else {
                    throw new XMLParseError(null,
                            "Cannot parse the element with the tag \"" + tag
                            + "\" into a comment child!");
                }
            }
        });
    }

    @Override
    public void writeXML(IOSIndentWriter out) {
        out.println("<Comment>").push();

        if (mGraphics != null) {
            mGraphics.writeXML(out);
        }

        out.println("<Text style=\"color:blue\">").push();
        formatHTML();
        out.println(mHTMLText.trim());
        out.pop().println("</Text>");
        out.pop().println("</Comment>");
    }

    @Override
    public CommentBoard getCopy() {
        return null;
    }
}
