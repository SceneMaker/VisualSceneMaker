package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.event.VariableChangedEvent;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.VariableEntry;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.font.TextLayout;

import java.awt.image.BufferedImage;
import java.text.AttributedString;
import java.util.ArrayList;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class VarBadgeLocal extends JComponent implements EventListener, ActionListener, Observer {

    public static final int FONT_SIZE = 11;
    public static final int PADDING_BETWEEN_LINE = 3;
    private final ArrayList<VariableEntry> mEntryList = new ArrayList<VariableEntry>();

    // TODO: Make format of variable badge as global preferences
    private final int mPositionOffset = 10;
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final SuperNode mSuperNode;

    // interaction flags
    // TODO: Make private
    public boolean mSelected;
    public boolean mDragged;
    private final JMenuItem mHideBadgeMenuItem;
    private final JMenuItem mShowBadgeMenuItem;
    private boolean mIsHidden;
    private boolean useCachedImage = false;
    private BufferedImage bufferedImage;

    public VarBadgeLocal(SuperNode superNode, boolean hidden) {
        mSuperNode = superNode;

        // Initialize the entry list
        SuperNode parentNode = mSuperNode;
        synchronized (mEntryList) {
            mEntryList.clear();

            ArrayList<VariableDefinition> varDefList = parentNode.getVarDefList();

            for (VariableDefinition varDef : varDefList) {
                mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                        TextFormat.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));
            }
        }
        // Initialize size and location
        setSize(new Dimension(1, 1));
        setLocation(superNode.getLocalVariableBadge().getPosition().getXPos(), superNode.getLocalVariableBadge().getPosition().getYPos());
        mIsHidden = hidden;
        mHideBadgeMenuItem = new JMenuItem("Hide");
        mHideBadgeMenuItem.addActionListener(this);
        mShowBadgeMenuItem = new JMenuItem("Show");
        mShowBadgeMenuItem.addActionListener(this);
    }

    private Dimension computeTextRectSize(Graphics2D graphics) {
        int width = 0, height = 0;

        synchronized (mEntryList) {
            for (VariableEntry entry : mEntryList) {
                TextLayout textLayout = new TextLayout(entry.getAttributed().getIterator(), graphics.getFontRenderContext());
                int advance = (int) textLayout.getVisibleAdvance();

                if (advance > width) {
                    width = advance;
                }

                int currentAll = (int) (textLayout.getAscent() + textLayout.getDescent() + textLayout.getLeading());

                height = height + currentAll;
            }
        }

        return new Dimension(width + 2 * mPositionOffset, height + 2 * mPositionOffset);
    }

    public boolean containsPoint(Point p) {
        return getBounds().contains(p.x, p.y);
    }

    @Override
    public synchronized void paintComponent(java.awt.Graphics g) {

        super.paintComponent(g);

        if (EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sSHOW_VARIABLE_BADGE_ON_WORKSPACE && !mEntryList.isEmpty()) {

            if (mIsHidden) {
                paintMinimized((Graphics2D) g);
                useCachedImage = false;
            } else {
                paintDetailedVariables((Graphics2D) g);

            }
        }
    }

    private void paintDetailedVariables(Graphics2D g) {
        // Enable antialiasing
        Dimension dimension = computeTextRectSize(g);

        if (useCachedImage == false) {

            bufferedImage = new BufferedImage((int) dimension.getWidth(), (int) dimension.getHeight(), BufferedImage.TYPE_4BYTE_ABGR);
            Graphics2D graphics = bufferedImage.createGraphics();
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            // Compute the size of the variable badge
            setSize(dimension);

            // draw background
            graphics.setColor(new Color(220, 220, 220, 200));
            graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);

            // Draw the variables
            graphics.setStroke(new BasicStroke(1.5f));
            graphics.setColor(Color.BLACK);

            // Draw Type Definitions and Variable Definition
            int currentDrawingOffset = 0;
            int y = 12;
            for (VariableEntry entry : mEntryList) {
                AttributedString attributedString = entry.getAttributed();
                TextLayout textLayout = new TextLayout(attributedString.getIterator(),
                        graphics.getFontRenderContext());

                currentDrawingOffset = (int) (currentDrawingOffset + textLayout.getAscent());
                graphics.drawString(attributedString.getIterator(), mPositionOffset,
                        mPositionOffset + currentDrawingOffset);
                y += FONT_SIZE + PADDING_BETWEEN_LINE;
                currentDrawingOffset = (int) (currentDrawingOffset + textLayout.getLeading()
                        + textLayout.getDescent());

            }
            paintFromImage(g);

            useCachedImage = true;
        } else {
            paintFromImage(g);
        }
    }

    private void paintFromImage(Graphics2D graphics2) {
        graphics2.drawImage(bufferedImage, 0, 0, null);
    }

    private void paintMinimized(Graphics2D g) {
        // Enable antialiasing
        Graphics2D graphics = g;

        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        // Compute the size of the variable badge
        Dimension dimension = new Dimension(128, 15);

        graphics.setColor(new Color(200, 200, 200, 100));
        setSize(dimension);

        // draw background
        graphics.fillRoundRect(0, 0, 15, 15, 5, 5);
        graphics.setColor(new Color(51, 51, 51));
        graphics.setFont(new Font("Serif", Font.PLAIN, FONT_SIZE));
        graphics.drawString("Local Variables [...]", 18, 12);
    }

    private boolean containsEntryFor(String varName) {
        for (VariableEntry entry : mEntryList) {
            if (entry.getVarName().equals(varName)) {
                return true;
            }
        }

        return false;
    }

    public ArrayList<VariableEntry> getEntryList() {
        return mEntryList;
    }

    // TODO: do we need this?
    public void mouseClicked(MouseEvent event) {
        mSelected = true;

        if ((event.getButton() == MouseEvent.BUTTON3) && (event.getClickCount() == 1)) {
            JPopupMenu menu = new JPopupMenu();

            if (mIsHidden) {
                menu.add(mShowBadgeMenuItem);
            } else {
                menu.add(mHideBadgeMenuItem);
            }

            menu.show(this, (int) (this.getAlignmentX() + this.getWidth()),
                    (int) (this.getAlignmentY() + this.getHeight()));
        }
    }

    // TODO: do we need this?
    public void mousePressed(MouseEvent e) {
        mSelected = true;
    }

    // TODO: do we need this?
    public void mouseReleased(MouseEvent e) {
        mDragged = false;
    }

    // TODO: do we need this?
    public void deSelect() {
        mSelected = false;
        mDragged = false;
    }

    public void updateLocation(Point vector) {
        // Set new location
        setLocation(new Point(getLocation().x + vector.x, getLocation().y + vector.y));
        // Set the location on data model
        mSuperNode.getLocalVariableBadge().setPosition(new NodePosition(getLocation().x, getLocation().y));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        if (source == mHideBadgeMenuItem) {
            setBadgeHidden(true);
        } else if (source == mShowBadgeMenuItem) {
            setBadgeHidden(false);
        }

        paintComponent(getGraphics());
    }

    public void setBadgeHidden(boolean value) {
        mSuperNode.hideLocalVarBadge(value);
        mIsHidden = value;
    }

    public boolean isHidden() {
        return mIsHidden;
    }

    @Override
    public void update(Observable o, Object obj) {

        // mLogger.message("VarBadge.update(" + obj + ")");
        // Clear the entry list
        synchronized (mEntryList) {
            mEntryList.clear();
            useCachedImage = false;
            // Recompute the entry list
            SuperNode parentNode = mSuperNode;

            for (VariableDefinition varDef : parentNode.getVarDefList()) {
//          String varName = varDef.getName();
                mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                        TextFormat.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));
            }
        }
    }

    @Override
    public synchronized void update(EventObject event) {
            if (event instanceof VariableChangedEvent) {
                updateVariable(((VariableChangedEvent) event).getVarValue());
                useCachedImage = false;
                // Editor.getInstance().update();
                revalidate();
                repaint(100);
            }
    }

    private void updateVariable(TPLTuple<String, String> varVal) {
        synchronized (mEntryList) {
            for (VariableEntry entry : mEntryList) {
                String var = entry.getVarName();    // the name of the current variable
                String typ = entry.getVarType();

                if (var.equals(varVal.getFirst())) {
                    TPLTuple<String, AttributedString> formatedPair = TextFormat.fillWithAttributes("#r#" + typ + " " + var
                            + " = " + varVal.getSecond());
                    entry.setFormatted(formatedPair.getFirst());
                    entry.setAttributed(formatedPair.getSecond());
                    entry.setHasChanged(true);
                }
            }
        }
    }
}
