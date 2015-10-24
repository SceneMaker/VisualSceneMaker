package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.sceneflow.graphics.comment.Rect;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import java.util.Observable;
import java.util.Observer;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

/**
 * @author Patrick Gebhard
 * @author Not me
 */
public class Comment extends JComponent implements EventListener, Observer, MouseListener, MouseMotionListener {
    private JEditorPane mTextEditor = null;
    private JLabel      mTextLabel  = null;

    // font
    private Font mFont = null;

    // position
    private Point mClickPosition     = new Point(0, 0);
    private Point mLastMousePosition = new Point(0, 0);

    // edit
    private boolean            mEditMode = false;
    private WorkSpacePanel          mWorkSpace;
    private EditorConfig mEditorConfig;

    // image
    private Image                               mResizeMarker;
    private AlphaComposite                      mAC;
    private AlphaComposite                      mACFull;
    private de.dfki.vsm.model.sceneflow.Comment mDataComment;

    // interaction flags
    public boolean mSelected;
    public boolean mPressed;
    public boolean mDragged;
    public boolean mResizing;
    public int     mXMovement;
    public int     mYMovement;

    public Comment() {
        mDataComment = null;
    }

    public Comment(WorkSpacePanel ws, de.dfki.vsm.model.sceneflow.Comment dataComment) {
        mAC          = AlphaComposite.getInstance(AlphaComposite.XOR, 0.15f);
        mACFull      = AlphaComposite.getInstance(AlphaComposite.SRC, 1.0f);
        mWorkSpace   = ws;
        mEditorConfig = mWorkSpace.getEditorConfig();
        mDataComment = dataComment;

        // resize marker
        mResizeMarker = ResourceLoader.loadImage("/res/img/new/resize.png");

        // font setup
        mFont = new Font("SansSerif", Font.ITALIC,    /* (mWorkSpace != null) ? */
                         mEditorConfig.sWORKSPACEFONTSIZE /* : sBUILDING_BLOCK_FONT_SIZE */);

        // size setup
        Rectangle rect = new Rectangle(mDataComment.getGraphics().getRect().getXPos(),
                                       mDataComment.getGraphics().getRect().getYPos(),
                                       mDataComment.getGraphics().getRect().getWidth(),
                                       mDataComment.getGraphics().getRect().getHeight());

        setBounds(rect);
        mTextLabel = new JLabel();
        mTextLabel.setOpaque(false);
        mTextLabel.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));
        mTextLabel.setVerticalAlignment(SwingConstants.TOP);
        mTextLabel.setFont(mFont);

        // mTextLabel.setForeground(new Color(147, 130, 52, 127));
        mTextLabel.setForeground(new Color(75, 75, 75, 127));
        mTextEditor = new JEditorPane();
        mTextEditor.setContentType(new HTMLEditorKit().getContentType());
        mTextEditor.setOpaque(false);
        mTextEditor.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        // now use the same font than the label!
        String bodyRule = "body { font-family: " + mFont.getFamily() + "; " + "font-size: " + mFont.getSize() + "pt; }";

        ((HTMLDocument) mTextEditor.getDocument()).getStyleSheet().addRule(bodyRule);
        setLayout(new BorderLayout());

        // first put it in the editor, then back in the label
        mTextEditor.setText(mDataComment.getHTMLText());
        mTextLabel.setText(mTextEditor.getText());
        add(mTextLabel, BorderLayout.CENTER);
    }

    @Override
    public void update(EventObject event) {   }

    
    @Override
    public void update(Observable o, Object obj) {
        update();
    }
    
    
    public void update() {
        
        mFont = new Font("SansSerif", Font.ITALIC, mEditorConfig.sWORKSPACEFONTSIZE);
        mTextLabel.setFont(mFont);
        mTextEditor.setFont(mFont);

        String bodyRule = "body { font-family: " + mFont.getFamily() + "; " + "font-size: " + mFont.getSize() + "pt; }";

        ((HTMLDocument) mTextEditor.getDocument()).getStyleSheet().addRule(bodyRule);
        mDataComment.setHTMLText(mTextEditor.getText());
        mTextEditor.setText(mDataComment.getHTMLText());
        mTextLabel.setText(mTextEditor.getText());
        repaint();
    }

    public String getDescription() {
        return toString();
    }

    public de.dfki.vsm.model.sceneflow.Comment getData() {
        return mDataComment;
    }

    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

//      mFont = new Font("SansSerif", Font.PLAIN, /*(mWorkSpace != null) ?*/ sWORKSPACEFONTSIZE /*: sBUILDING_BLOCK_FONT_SIZE*/);
//           mTextLabel.setFont(mFont);
//             mTextLabel.setText(mTextEditor.getText());
//    S tring bodyRule = "body { font-family: " + mFont.getFamily() + "; " + "font-size: " + mFont.getSize() + "pt; }";
//    ( (HTMLDocument) mTextEditor.getDocument()).getStyleSheet().addRule(bodyRule);
        Graphics2D graphics = (Graphics2D) g;

        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        Rectangle r = getBounds();

        // graphics.setColor(new Color(227, 206, 29, 127));
        graphics.setColor(new Color(200, 200, 200, 200));

        if (mEditMode) {
            graphics.setStroke(new BasicStroke(2.0f));
            graphics.drawRoundRect(0, 0, r.width - 1, r.height - 1, 15, 15);
        } else {
            graphics.fillRoundRect(0, 0, r.width, r.height, 15, 15);
            graphics.setComposite(mAC);
            graphics.drawImage(mResizeMarker, r.width - 13, r.height - 13, this);
            graphics.setComposite(mACFull);
        }
    }

    public void resize(Point p) {
        Rectangle r = getBounds();

        if (!((r.width <= 50) && (p.x < 0))) {
            r.width = r.width + p.x;
            r.width = (r.width < 50)
                      ? 50
                      : r.width;
        }

        if (!((r.height <= 50) && (p.y < 0))) {
            r.height = r.height + p.y;
            r.height = (r.height < 50)
                       ? 50
                       : r.height;
        }

        setBounds(r);
        setSize(r.width, r.height);

        // update data
        Rectangle r2 = getBounds();

        mDataComment.getGraphics().setRect(new Rect(r2.x, r2.y, r2.width, r2.height));

        // DEBUG System.out.println("size " + getBounds());
    }

    public boolean containsPoint(Point p) {
        return getBounds().contains(p.x, p.y);
    }

    /*
     * Returns true if mouse in the lower right area, which stands for the resizng area.
     *  --------
     * |        |
     * |        |
     * |        |
     * |       -|
     * |      | |
     * |--------
     *
     */
    public boolean isResizingAreaSelected(Point p) {
        Rectangle r = getBounds();

        // DEBUG System.out.println("bounds " + getBounds());
        // DEBUG System.out.println("point " + p);
        if (((r.x + r.width) - p.x < 15) && ((r.y + r.height) - p.y < 15)) {
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void mouseClicked(MouseEvent e) {

        // DEBUG System.out.println("mouse clicked");
        mPressed  = false;
        mSelected = true;

        Point loc      = getLocation();
        Point clickLoc = e.getPoint();

        mLastMousePosition = new Point(clickLoc);

        // save click location relavitvely to node postion
        mClickPosition.setLocation(clickLoc.x - loc.x, clickLoc.y - loc.y);

        if ((e.getButton() == MouseEvent.BUTTON1) && (e.getClickCount() == 2)) {

            // DEBUG System.out.println("double click");
            String text = mTextLabel.getText();

            mTextEditor.setText(text);
            remove(mTextLabel);
            add(mTextEditor, BorderLayout.CENTER);
            mEditMode = true;
        }

        // show contect menu
        if ((e.getButton() == MouseEvent.BUTTON3) && (e.getClickCount() == 1)) {
            mWorkSpace.showContextMenu(e, this);
        }

        revalidate();
        repaint();
    }

    @Override
    public void mousePressed(MouseEvent e) {

        // DEBUG System.out.println("mouse pressed");
        mPressed  = true;
        mSelected = true;

        Point loc      = getLocation();
        Point clickLoc = e.getPoint();

        mLastMousePosition = new Point(clickLoc);

        // save click location relavitvely to node postion
        mClickPosition.setLocation(clickLoc.x - loc.x, clickLoc.y - loc.y);
    }

    @Override
    public void mouseReleased(MouseEvent e) {

        // DEBUG System.out.println("mouse released");
        mPressed           = false;
        mDragged           = false;
        mLastMousePosition = new Point(0, 0);
        repaint();
    }

    @Override
    public void mouseEntered(MouseEvent e) {}

    @Override
    public void mouseExited(MouseEvent e) {}

    @Override
    public void mouseDragged(MouseEvent e) {}

    @Override
    public void mouseMoved(MouseEvent e) {}

    /*
     * Resets the comment to its default visual behavior
     */
    public void setDeselected() {

        // DEBUG System.out.println("Comment Deselected!");
        if (mEditMode) {
            String htmlText = mTextEditor.getText();

            System.out.println(htmlText);
            mTextLabel.setText(htmlText);
            remove(mTextEditor);
            add(mTextLabel, BorderLayout.CENTER);
            mEditMode = false;

            // store text in TEXT node
            mDataComment.setHTMLText(htmlText);
        }

        mSelected = false;
        mPressed  = false;
        mDragged  = false;
        mResizing = false;
        repaint();
        update();
    }

    public synchronized void updateLocation(Point mouseMovementVector) {
        Point currentNodeLocation = getLocation();

        mXMovement = mouseMovementVector.x;
        mYMovement = mouseMovementVector.y;

        Point finalLocation = new Point(currentNodeLocation.x + mXMovement, currentNodeLocation.y + mYMovement);

        setLocation(finalLocation);

        // update data
        Rectangle r = getBounds();

        mDataComment.getGraphics().setRect(new Rect(r.x, r.y, r.width, r.height));
    }
}
