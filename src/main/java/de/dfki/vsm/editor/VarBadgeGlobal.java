package de.dfki.vsm.editor;

import de.dfki.vsm.event.EventListener;
import de.dfki.vsm.event.EventObject;
import de.dfki.vsm.event.event.VariableChangedEvent;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.chart.badge.VariableEntry;
import de.dfki.vsm.model.sceneflow.chart.graphics.node.NodePosition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.util.TextFormatDesktop;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.Tuple;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.font.TextAttribute;
import java.awt.font.TextLayout;
import java.text.AttributedString;
import java.util.*;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class VarBadgeGlobal extends JComponent implements EventListener, ActionListener, Observer {

    //
    private final ArrayList<VariableEntry> mEntryList = new ArrayList<>();

    // TODO: Make format of variable badge as global preferences
    private final int mPositionOffset = 10;
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // The supernode
    private final SuperNode mSuperNode;

    // interaction flags
    // TODO: Make private
    public boolean mSelected;
    public boolean mDragged;

    //
    private final JMenuItem mHideBadgeMenuItem;
    private final JMenuItem mShowBadgeMenuItem;
    private boolean mIsHidden;


    public VarBadgeGlobal(SuperNode superNode, boolean hidden) {

        // mWorkSpace = ws;
        mSuperNode = superNode;

        // Initialize the entry list
        SuperNode parentNode = mSuperNode.getParentNode();


        synchronized (mEntryList) {
            mEntryList.clear();

            while (parentNode != null) {
                ArrayList<VariableDefinition> varDefList = parentNode.getVarDefList();

                for (VariableDefinition varDef : varDefList) {
                    mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                            TextFormatDesktop.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));
                }

                parentNode = parentNode.getParentNode();
            }
        }

        // Initialize font
        Map<TextAttribute, Object> map = new Hashtable<>();

        map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
        map.put(TextAttribute.FAMILY, Font.SANS_SERIF);
        map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
        map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
        map.put(TextAttribute.SIZE, 16); // get from Editor config

        // Derive the font from the attribute map
        Font font = Font.getFont(map);

        // Derive the node's font metrics from the font
        FontMetrics fontMetrics = getFontMetrics(font);

        // Set the node's font to the updated font
        setFont(font);

        // Initialize size and location
        setSize(new Dimension(1, 1));     
        setLocation(superNode.getGlobalVariableBadge().getPosition().getXPos(),
                superNode.getGlobalVariableBadge().getPosition().getYPos());
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
    public void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);

        if (EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject().getEditorConfig().sSHOW_VARIABLE_BADGE_ON_WORKSPACE && !mEntryList.isEmpty()) {
            Graphics2D graphics = (Graphics2D) g;

            // Enable antialiasing
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
            graphics.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
            graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);

            if (mIsHidden) {
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
                    TextLayout textLayout = new TextLayout(attributedString.getIterator(),
                            graphics.getFontRenderContext());

                    currentDrawingOffset = (int) (currentDrawingOffset + textLayout.getAscent());
                    graphics.drawString(attributedString.getIterator(), mPositionOffset,
                            mPositionOffset + currentDrawingOffset);
                    currentDrawingOffset = (int) (currentDrawingOffset + textLayout.getLeading() + textLayout.getDescent());
                }
            }
        }
    }

    private boolean containsEntryFor(String varName) {
        synchronized (mEntryList) {
            for (VariableEntry entry : mEntryList) {
                if (entry.getVarName().equals(varName)) {
                    return true;
                }
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
        mDragged = false;
    }

    public void updateLocation(Point vector) {
        // Set new location
        setLocation(new Point(getLocation().x + vector.x, getLocation().y + vector.y));
        // Set the location on data model
        mSuperNode.getGlobalVariableBadge().setPosition(new NodePosition(getLocation().x, getLocation().y));
    }

    @Override
    public void update(Observable o, Object obj) {

        synchronized (mEntryList) {
            // mLogger.message("VarBadge.update(" + obj + ")");
            // Clear the entry list
            mEntryList.clear();

            // Recompute the entry list
            SuperNode parentNode = mSuperNode.getParentNode();

            while (parentNode != null) {
                for (VariableDefinition varDef : parentNode.getVarDefList()) {
                    String varName = varDef.getName();

                    // if (!containsEntryFor(varName)) {
                    mEntryList.add(new VariableEntry(parentNode, false, varDef.getConcreteSyntax(), varDef.getFormattedSyntax(),
                            TextFormatDesktop.fillWithAttributes(varDef.getFormattedSyntax()).getSecond()));

                    // }
                }

                parentNode = parentNode.getParentNode();
            }
        }
    }

    @Override
    public synchronized void update(EventObject event) {
        // Update the font and the font metrics that have to be
        // recomputed if the node's font size has changed
        // TODO: Move attributes to preferences and make editable
        Map<TextAttribute, Object> map = new Hashtable<>();

        map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
        map.put(TextAttribute.FAMILY, Font.SANS_SERIF);
        map.put(TextAttribute.POSTURE, TextAttribute.POSTURE_OBLIQUE);
        map.put(TextAttribute.WEIGHT, TextAttribute.WEIGHT_DEMIBOLD);
        map.put(TextAttribute.SIZE, 16); // get from Editor config

        // Derive the font from the attribute map
        Font font = Font.getFont(map);

        // Derive the node's font metrics from the font
        FontMetrics fontMetrics = getFontMetrics(font);

        // Set the node's font to the updated font
        setFont(font);

        if (event instanceof VariableChangedEvent) {
            updateVariable(((VariableChangedEvent) event).getVarValue());

            // Editor.getInstance().update();
            revalidate();
            repaint(100);
        }
    }

    private void updateVariable(Tuple<String, String> varVal) {
        synchronized (mEntryList) {
            // System.err.println("updateVariable");
            for (VariableEntry entry : mEntryList) {
                String var = entry.getVarName();    // the name of the current variable
                String typ = entry.getVarType();

                if (var.equals(varVal.getFirst())) {
                    Tuple<String, AttributedString> formatedPair = TextFormatDesktop.fillWithAttributes("#r#" + typ + " " + var
                            + " = " + varVal.getSecond());

                    entry.setFormatted(formatedPair.getFirst());
                    entry.setAttributed(formatedPair.getSecond());
                    entry.setHasChanged(true);
                }
            }
        }
    }
}
