package de.dfki.vsm.model.sceneflow;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.model.ModelObject;
import de.dfki.vsm.model.sceneflow.graphics.comment.CommentGraphics;
import de.dfki.vsm.util.ios.IOSIndentWriter;
import de.dfki.vsm.util.xml.XMLParseAction;
import de.dfki.vsm.util.xml.XMLParseError;

import org.w3c.dom.Element;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Font;

import javax.swing.JEditorPane;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class Comment implements ModelObject {

    protected SuperNode mParentNode = null;
    protected String mHTMLText = "";
    protected CommentGraphics mGraphics;
    protected int mFontSize;
    JEditorPane mTextEditor;

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

    private void formatHTML() {
        // PG 30.7.2016 Da fuck? This is not allowed in the model!      
        //mFontSize = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sWORKSPACEFONTSIZE;
        mFontSize = 16;

        if (mTextEditor == null) {
            mTextEditor = new JEditorPane();
        }

        //mTextEditor.setContentType(new HTMLEditorKit().getContentType());
        // now use the same font than the label!
        Font mFont = new Font("SansSerif", Font.PLAIN, mFontSize);
        String bodyRule = "body { font-family: " + mFont.getFamily() + "; " + "font-size: " + mFont.getSize() + "pt; }";

        // ((HTMLDocument) mTextEditor.getDocument()).getStyleSheet().addRule(bodyRule);
        mTextEditor.setText(mHTMLText);
        mHTMLText = mTextEditor.getText();
    }

    public void parseXML(Element element) throws XMLParseError {
        XMLParseAction.processChildNodes(element, new XMLParseAction() {
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

    public SyntaxObject getCopy() {
        return null;
    }
}
