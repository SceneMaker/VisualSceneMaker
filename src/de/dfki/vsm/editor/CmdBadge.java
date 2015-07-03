package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.config.ProjectPreferences;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.font.TextLayout;

import java.text.AttributedString;

import java.util.ArrayList;
import java.util.Observer;
import java.util.Timer;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public class CmdBadge extends JComponent implements EventListener, Observer {

    //
    private final LOGDefaultLogger mLogger      = LOGDefaultLogger.getInstance();
    private final EventCaster      mEventCaster = EventCaster.getInstance();

    // edit
    private boolean      mEditMode = false;
    private final Action wrapper   = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            setDeselected();
        }
    };

    // The node to which the badge is connected
    private final Node               mNode;
    private final ProjectPreferences mPreferences;
    private final Timer              mVisuTimer;

    // The maintained list
    private ArrayList<TPLTuple<String, AttributedString>> mStringList;
    private final ArrayList<JTextArea>                    mCmdEditors;
    private final Font                                    mFont;

    /**
     *
     *
     *
     *
     *
     */
    public CmdBadge(Node node) {
        mNode        = node;
        mPreferences = node.getWorkSpace().getPreferences();
        mVisuTimer   = new Timer("Command-Badge-Visualization-Timer");
        setSize(new Dimension(1, 1));
        setLocation(0, 0);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        mFont       = new Font("SansSerif", Font.ITALIC,    /* (mWorkSpace != null) ? */
                               mPreferences.sWORKSPACEFONTSIZE /* : sBUILDING_BLOCK_FONT_SIZE */);
        mCmdEditors = new ArrayList<>();
        update();
    }

    /**
     *
     *
     *
     *
     *
     */
    @Override
    public void update(java.util.Observable obs, Object obj) {

        // mLogger.message("CmdBadge.update(" + obj + ")");
        update();
    }

    private void update() {
        ArrayList<String> strings      = new ArrayList<>();
        Vector<Command>   nodeCommands = mNode.getDataNode().getCmdList();

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
        int width  = 0,
            height = 0;

        for (int i = 0; i < mStringList.size(); i++) {
            TextLayout textLayout = new TextLayout(mStringList.get(i).getSecond().getIterator(),
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

        // mVisuTimer = null;
    }

    /*
     * Implements ActivityListener
     */
    @Override
    public void update(EventObject event) {
        if (mVisuTimer != null) {
            if (event instanceof SceneExecutedEvent) {

                // TODO: is getName right?
                // Do visualization over data model
                String  sceneName = ((SceneExecutedEvent) event).getScene().getName();
                boolean contained = false;

                for (TPLTuple<String, AttributedString> pair : mStringList) {
                    if (pair.getFirst().equals("PlaySceneGroup ( \"" + sceneName + "\" )")) {
                        contained = true;
                    }
                }

                if (contained) {
                    VisualisationTask visuTask = new VisualisationTask(mPreferences.sVISUALISATIONTIME, this);

                    mVisuTimer.schedule(visuTask, 0, 25);
                }
            }
        }
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        if (mEditMode) {
            super.paintComponent(g);

            Graphics2D graphics = (Graphics2D) g;

            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            Dimension dimension = computeTextRectSize(graphics);

            dimension = new Dimension((int) (dimension.width * 1.2), (int) (22 * (mCmdEditors.size())));
            setSize(dimension);
            setLocation(mNode.getLocation().x + (mPreferences.sNODEWIDTH / 2) - (dimension.width / 2),
                        mNode.getLocation().y + mPreferences.sNODEHEIGHT);

            // draw background
            graphics.setColor(new Color(155, 155, 155, 100));
            graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
            graphics.setStroke(new BasicStroke(1.5f));
            graphics.setColor(Color.BLACK);
        } else {

            // System.err.println("Painting badge");
            super.paintComponent(g);

            Graphics2D graphics = (Graphics2D) g;

            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            Dimension dimension = computeTextRectSize(graphics);

            setSize(dimension);
            setLocation(mNode.getLocation().x + (mPreferences.sNODEWIDTH / 2) - (dimension.width / 2),
                        mNode.getLocation().y + mPreferences.sNODEHEIGHT);

            // draw background
            graphics.setColor(new Color(100, 100, 100, 100));
            graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);

            //
            // if (mVisualisationTask != null) {
            // if (mVisualisationTask.getActivityTime() > 20) {
            // graphics.setColor(new Color(246, 0, 0, 100));
            // graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
            // } else {
            // graphics.setColor(new Color(246, 0, 0, 100 - (100 - 5 * mVisualisationTask.getActivityTime())));
            // graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
            // }
            // }
            graphics.setStroke(new BasicStroke(1.5f));
            graphics.setColor(Color.BLACK);

            // Draw Type Definitions and Variable Definition
            int currentDrawingOffset = 0;

            for (TPLTuple<String, AttributedString> pair : mStringList) {
                AttributedString attributedString = pair.getSecond();
                TextLayout       textLayout       = new TextLayout(attributedString.getIterator(),
                                                        graphics.getFontRenderContext());

                currentDrawingOffset = currentDrawingOffset + (int) textLayout.getAscent();
                graphics.drawString(attributedString.getIterator(), 5, 5 + currentDrawingOffset);
                currentDrawingOffset = currentDrawingOffset + (int) textLayout.getLeading()
                                       + (int) textLayout.getDescent();
            }
        }
    }

    private void addCmdEditor(String text) {
        JTextArea cmdEditor = new JTextArea();

        cmdEditor.setFont(mFont);
        cmdEditor.setText(text);
        cmdEditor.setOpaque(false);
        cmdEditor.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        KeyStroke keyStroke = KeyStroke.getKeyStroke("ENTER");
        Object    actionKey = cmdEditor.getInputMap(JComponent.WHEN_FOCUSED).get(keyStroke);

        cmdEditor.getActionMap().put(actionKey, wrapper);
        mCmdEditors.add(cmdEditor);
    }

    public void setSelected() {
        mEditMode = true;

        for (TPLTuple<String, AttributedString> s : mStringList) {
            addCmdEditor(s.getFirst());
        }

        for (JTextArea editor : mCmdEditors) {
            add(editor, BorderLayout.CENTER);
        }

        mCmdEditors.get(0).requestFocusInWindow();
    }

    public boolean containsPoint(int x, int y) {
        return getBounds().contains(x, y);
    }

    /*
     * Resets the badge to its default visual behavior
     */
    public synchronized void setDeselected() {
        String          text;
        Vector<Command> copyOfCmdList = new Vector<>();

        if (mEditMode) {
            for (int i = 0; i < mStringList.size(); i++) {
                text = mCmdEditors.get(i).getText();

                if (!text.equals("")) {
                    Command command;

                    try {
                        _SFSLParser_.parseResultType = _SFSLParser_.CMD;
                        _SFSLParser_.run(text);

                        Command cmd = _SFSLParser_.cmdResult;

                        if ((cmd != null) &&!_SFSLParser_.errorFlag) {
                            command = cmd;
                        } else {
                            return;
                        }
                    } catch (Exception e) {
                        return;
                    }

                    copyOfCmdList.add(command);
                }
            }

            for (JTextArea editor : mCmdEditors) {
                remove(editor);
            }

            mCmdEditors.removeAll(mCmdEditors);
            mNode.getDataNode().setCmdList(copyOfCmdList);
            mEditMode = false;
        }

        repaint();
        update();
    }
}
