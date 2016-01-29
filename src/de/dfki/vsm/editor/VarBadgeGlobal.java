package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.VariableChangedEvent;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.VariableEntry;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.graphics.Position;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.font.TextLayout;

import java.text.AttributedString;

import java.util.Observable;
import java.util.Observer;
import java.util.Vector;

import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class VarBadgeGlobal extends JComponent implements EventListener, ActionListener, Observer {

    //
    private final Vector<VariableEntry> mEntryList = new Vector<VariableEntry>();

    // TODO: Make format of variable badge as global preferences
    private final int              mPositionOffset = 10;
    private final LOGDefaultLogger mLogger         = LOGDefaultLogger.getInstance();

    // The supernode
    private final SuperNode mSuperNode;

    // interaction flags
    // TODO: Make private
    public boolean mSelected;
    public boolean mDragged;

    //
    private final JMenuItem mHideBadgeMenuItem;
    private final JMenuItem mShowBadgeMenuItem;
    private boolean         mIsHidden;

    public VarBadgeGlobal(SuperNode superNode, boolean hidden) {

        // mWorkSpace = ws;
        mSuperNode = superNode;

        // Initialize the entry list
        SuperNode parentNode = mSuperNode.getParentNode();

        mEntryList.clear();

        while (parentNode != null) {
            Vector<VarDef> varDefList = parentNode.getVarDefList();

            for (VarDef varDef : varDefList) {
                mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                                         TextFormat.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));
            }

            parentNode = parentNode.getParentNode();
        }

        // Initialize size and location
        setSize(new Dimension(1, 1));
        setLocation(superNode.getGlobalVariableBadge().getPosition().getXPos(),
                    superNode.getGlobalVariableBadge().getPosition().getYPos());
        mIsHidden          = hidden;
        mHideBadgeMenuItem = new JMenuItem("Hide");
        mHideBadgeMenuItem.addActionListener(this);
        mShowBadgeMenuItem = new JMenuItem("Show");
        mShowBadgeMenuItem.addActionListener(this);
    }

    private Dimension computeTextRectSize(Graphics2D graphics) {
        int width  = 0,
            height = 0;

        for (VariableEntry entry : mEntryList) {
            TextLayout textLayout = new TextLayout(entry.getAttributed().getIterator(), graphics.getFontRenderContext());
            int        advance    = (int) textLayout.getVisibleAdvance();

            if (advance > width) {
                width = advance;
            }

            int currentAll = (int) (textLayout.getAscent() + textLayout.getDescent() + textLayout.getLeading());

            height = height + currentAll;
        }

        return new Dimension(width + 2 * mPositionOffset, height + 2 * mPositionOffset);
    }

    public boolean containsPoint(Point p) {
        return getBounds().contains(p.x, p.y);
    }

    @Override
    public synchronized void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);

        if (EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sSHOW_VARIABLE_BADGE_ON_WORKSPACE &&!mEntryList.isEmpty()) {
            if (mIsHidden) {

                // Enable antialiasing
                Graphics2D graphics = (Graphics2D) g;

                graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Compute the size of the variable badge
                Dimension dimension = new Dimension(135, 15);

                graphics.setColor(new Color(50, 50, 50, 100));
                setSize(dimension);

                // draw background
                graphics.fillRoundRect(0, 0, 15, 15, 5, 5);
                graphics.setColor(new Color(51, 51, 51));
                graphics.setFont(new Font("Serif", Font.PLAIN, 11));
                graphics.drawString("Global Variables [...]", 18, 12);
            } else {

                // Enable antialiasing
                Graphics2D graphics = (Graphics2D) g;

                graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Compute the size of the variable badge
                Dimension dimension = computeTextRectSize(graphics);

                setSize(dimension);

                // draw background
                graphics.setColor(new Color(200, 200, 200, 200));
                graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);

                // Draw the variables
                graphics.setStroke(new BasicStroke(1.5f));
                graphics.setColor(Color.BLACK);

                // Draw Type Definitions and Variable Definition
                int currentDrawingOffset = 0;

                for (VariableEntry entry : mEntryList) {
                    AttributedString attributedString = entry.getAttributed();
                    TextLayout       textLayout       = new TextLayout(attributedString.getIterator(),
                                                            graphics.getFontRenderContext());

                    currentDrawingOffset = currentDrawingOffset + (int) textLayout.getAscent();
                    graphics.drawString(attributedString.getIterator(), mPositionOffset,
                                        mPositionOffset + currentDrawingOffset);
                    currentDrawingOffset = currentDrawingOffset + (int) textLayout.getLeading()
                                           + (int) textLayout.getDescent();
                }
            }
        }
    }

    private boolean containsEntryFor(String varName) {
        for (VariableEntry entry : mEntryList) {
            if (entry.getVarName().equals(varName)) {
                return true;
            }
        }

        return false;
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
        mSuperNode.hideGlobalVarBadge(value);
        mIsHidden = value;
    }

    public boolean isHidden() {
        return mIsHidden;
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
        mDragged  = false;
    }

    public void updateLocation(Point vector) {

        // Set new location
        setLocation(new Point(getLocation().x + vector.x, getLocation().y + vector.y));

        // Set the location on data model
        mSuperNode.getGlobalVariableBadge().setPosition(new Position(getLocation().x, getLocation().y));
    }
    
    @Override
    public void update(Observable o, Object obj) {

        // mLogger.message("VarBadge.update(" + obj + ")");
        // Clear the entry list
        mEntryList.clear();

        // Recompute the entry list
        SuperNode parentNode = mSuperNode.getParentNode();

        while (parentNode != null) {
            for (VarDef varDef : parentNode.getVarDefList()) {
                String varName = varDef.getName();

                // if (!containsEntryFor(varName)) {
                mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                                         TextFormat.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));

                // }
            }

            parentNode = parentNode.getParentNode();
        }
    }

    @Override
    public synchronized void update(EventObject event) {
        if (event instanceof VariableChangedEvent) {
            updateVariable(((VariableChangedEvent) event).getVarValue());

            // Editor.getInstance().update();
            revalidate();
            repaint();
        }
    }
    
    private void updateVariable(TPLTuple<String, String> varVal) {
        
        synchronized(mEntryList) {
            
            
            // System.err.println("updateVariable");
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
