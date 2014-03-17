package de.dfki.vsm.editor;

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import static de.dfki.vsm.editor.util.Preferences.sNODEHEIGHT;
import static de.dfki.vsm.editor.util.Preferences.sNODEWIDTH;
import static de.dfki.vsm.editor.util.Preferences.sVISUALISATIONTIME;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.TextLayout;
import java.text.AttributedString;
import java.util.ArrayList;
import java.util.Observer;
import java.util.Timer;
import java.util.Vector;
import javax.swing.JComponent;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class CmdBadge extends JComponent implements EventListener, Observer {

    // The node to which the badge is connected
    private final Node mNode;
    private final Timer mVisuTimer;
    // The maintained list
    private ArrayList<TPLTuple<String, AttributedString>> mStringList;
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private final EventCaster mEventCaster = EventCaster.getInstance();

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public void update(java.util.Observable obs, Object obj) {
        //mLogger.message("CmdBadge.update(" + obj + ")");
        update();
    }

    /**
     * *************************************************************************
     *
     *
     *
     *************************************************************************
     */
    public CmdBadge(Node node) {
        mNode = node;
        mVisuTimer = new Timer("Command-Badge-Visualization-Timer");
        setSize(new Dimension(1, 1));
        setLocation(0, 0);
        update();
    }

    private void update() {
        ArrayList<String> strings = new ArrayList<String>();
        Vector<Command> nodeCommands = mNode.getDataNode().getCmdList();
        if ((nodeCommands != null) && (nodeCommands.size() > 0)) {
            for (Command cmd : nodeCommands) {
                strings.add(((Command) cmd).getFormattedSyntax());
            }
        }
        // Update the string list
        mStringList = TextFormat.getPairList(strings);
        // Sets visibility of the component to true only if there is something to display
        setVisible(!mStringList.isEmpty());
    }

    private Dimension computeTextRectSize(Graphics2D graphics) {
        int width = 0, height = 0;
        for (int i = 0; i < mStringList.size(); i++) {
            TextLayout textLayout = new TextLayout(
                    mStringList.get(i).getSecond().getIterator(),
                    graphics.getFontRenderContext());
            int advance = (int) textLayout.getVisibleAdvance();
            if (advance > width) {
                width = advance;
            }
            int currentAll = (int) (textLayout.getAscent() + textLayout.getDescent() + textLayout.getLeading());
            height = height + currentAll;
        }
        return new Dimension(width + 2 * 5, height + 2 * 5);
    }

    /**
     * Nullifies the VisalisationTimer thread
     */
    public void stopVisualisation() {
        mVisuTimer.purge();
        mVisuTimer.cancel();
        //  mVisuTimer = null;
    }

    /*
     * Implements ActivityListener
     */
    public void update(EventObject event) {
        if (mVisuTimer != null) {
            if (event instanceof SceneExecutedEvent) {
                // TODO: is getName right?
                // Do visualization over data model
                String sceneName = ((SceneExecutedEvent) event).getScene().getName();
                boolean contained = false;
                for (TPLTuple<String, AttributedString> pair : mStringList) {
                    if (pair.getFirst().equals("PlaySceneGroup ( \"" + sceneName + "\" )")) {
                        contained = true;
                    }
                }
                if (contained) {
                    VisualisationTask visuTask = new VisualisationTask(sVISUALISATIONTIME, this);
                    mVisuTimer.schedule(visuTask, 0, 25);
                }
            }
        }
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        // System.err.println("Painting badge");
        super.paintComponent(g);
        Graphics2D graphics = (Graphics2D) g;
        graphics.setRenderingHint(
                RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        Dimension dimension = computeTextRectSize(graphics);
        setSize(dimension);
        setLocation(
                mNode.getLocation().x + (sNODEWIDTH / 2) - (dimension.width / 2),
                mNode.getLocation().y + sNODEHEIGHT);
        // draw background
        graphics.setColor(new Color(100, 100, 100, 100));
        graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
        //
//        if (mVisualisationTask != null) {
//            if (mVisualisationTask.getActivityTime() > 20) {
//                graphics.setColor(new Color(246, 0, 0, 100));
//                graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
//            } else {
//                graphics.setColor(new Color(246, 0, 0, 100 - (100 - 5 * mVisualisationTask.getActivityTime())));
//                graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
//            }
//        }

        graphics.setStroke(new BasicStroke(1.5f));
        graphics.setColor(Color.BLACK);
        // Draw Type Definitions and Variable Definition
        int currentDrawingOffset = 0;
        for (TPLTuple<String, AttributedString> pair : mStringList) {
            AttributedString attributedString = pair.getSecond();
            TextLayout textLayout = new TextLayout(
                    attributedString.getIterator(),
                    graphics.getFontRenderContext());
            currentDrawingOffset = currentDrawingOffset + (int) textLayout.getAscent();
            graphics.drawString(attributedString.getIterator(), 5, 5 + currentDrawingOffset);
            currentDrawingOffset = currentDrawingOffset + (int) textLayout.getLeading() + (int) textLayout.getDescent();
        }

    }
}
