package de.dfki.vsm.editor;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.event.ProjectChangedEvent;
import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.util.VisualisationTask;
import de.dfki.vsm.model.project.EditorConfig;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.TextFormat;
import de.dfki.vsm.util.evt.EventDispatcher;
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
import java.awt.Point;
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
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class CmdBadge extends JComponent implements EventListener, Observer {

    //
    private final LOGDefaultLogger mLogger      = LOGDefaultLogger.getInstance();
    private final EventDispatcher  mEventCaster = EventDispatcher.getInstance();

    // edit
    private boolean      mEditMode = false;
    private final Action wrapper   = new AbstractAction() {
        @Override
        public void actionPerformed(ActionEvent e) {
            endEditMode();
        }
    };

    // The node to which the badge is connected
    private final Node               mNode;
    private final EditorConfig       mEditorConfig;
    private final Timer              mVisuTimer;

    // The maintained list
    private ArrayList<TPLTuple<String, AttributedString>> mStringList;
    private final ArrayList<JTextArea>                    mCmdEditors;
    private final Font                                    mFont;

    /**
     *
     *
     */
    public CmdBadge(Node node) {
        mNode           = node;
        mEditorConfig   = mNode.getWorkSpace().getEditorConfig();
        mVisuTimer      = new Timer("Command-Badge-Visualization-Timer");
        mFont           = new Font("Monospaced", 
                                    Font.ITALIC, 
                                    mEditorConfig.sWORKSPACEFONTSIZE);
        mCmdEditors     = new ArrayList<>();
        
        setSize(new Dimension(1, 1));
        setLocation(0, 0);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        update();
    }

    @Override
    public void paintComponent(java.awt.Graphics g) {
        super.paintComponent(g);
        
//        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();
//        for(StackTraceElement st: stackTraceElements)
//        {
//            System.out.println(st.getClassName()+ "-----" + st.getFileName()+"-----"+st.getme+"-----"+st.getLineNumber());
//        }
        
        Graphics2D graphics = (Graphics2D) g;
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        Dimension dimension;
            
        if (mEditMode) {
            
            dimension = new Dimension((int) (10 + getEditorWidth() * mFont.getSize()/1.5), (int) (30 * (mCmdEditors.size())));
            // draw background
            graphics.setColor(new Color(155, 155, 155, 100));
             
        } else {
            
            dimension = computeTextRectSize(graphics);
            // draw background
            graphics.setColor(new Color(100, 100, 100, 100));

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
        
        setSize(dimension);
        setLocation( mNode.getLocation().x + (mEditorConfig.sNODEWIDTH / 2) - (dimension.width / 2),
                     mNode.getLocation().y + mEditorConfig.sNODEHEIGHT);
        graphics.fillRoundRect(0, 0, dimension.width, dimension.height, 5, 5);
        graphics.setStroke(new BasicStroke(1.5f));
        graphics.setColor(Color.BLACK);
          
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

  
    private int getEditorWidth(){
        int width = 0;
        for(JTextArea i: mCmdEditors){
            if(i.getText().length()>width){
                width = i.getText().length();
            }  
        }
        return width;
    }
    
    public void setEditMode() {
        mEditMode = true;
        for (TPLTuple<String, AttributedString> s : mStringList) {
            addCmdEditor(s.getFirst());
        }

        for (JTextArea editor : mCmdEditors) {
            add(editor, BorderLayout.CENTER);
        }
        mEventCaster.convey(new NodeSelectedEvent(this, mNode.getDataNode()));
        mCmdEditors.get(0).requestFocusInWindow();
    }
    
     /*
     * Resets badge to its default visual behavior
     */
    public synchronized void endEditMode() {
        Vector<Command> copyOfCmdList = new Vector<>();

        if (mEditMode) {
            for (int i = 0; i < mStringList.size(); i++) {
                String text = mCmdEditors.get(i).getText();

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
                        mCmdEditors.get(i).setForeground(Color.red);
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
            mEventCaster.convey(new ProjectChangedEvent(this));
            mEventCaster.convey(new NodeSelectedEvent(this, mNode.getDataNode()));
            mEditMode = false;
          
            
        }

        repaint();
        update();
    }
    
     
    private void addCmdEditor(String text) {
        JTextArea cmdEditor = new JTextArea();
        
        cmdEditor.getDocument().addDocumentListener(new DocumentListener() {

            @Override
            public void insertUpdate(DocumentEvent e) {
                cmdEditor.setForeground(Color.black);
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                cmdEditor.setForeground(Color.black);
             }

            @Override
            public void changedUpdate(DocumentEvent e) {
                
            }
    });

        cmdEditor.setFont(mFont);
        cmdEditor.setText(text);
        cmdEditor.setOpaque(false);
        cmdEditor.setBorder(BorderFactory.createEmptyBorder(4, 4, 4, 4));

        KeyStroke keyStroke = KeyStroke.getKeyStroke("ENTER");
        Object    actionKey = cmdEditor.getInputMap(JComponent.WHEN_FOCUSED).get(keyStroke);

        cmdEditor.getActionMap().put(actionKey, wrapper);
        mCmdEditors.add(cmdEditor);
    }

    
    public void updateLocation(Point vector) {
        Point location = getLocation();
        setLocation(location.x + vector.x, location.y + vector.y);
    }
    
    
    public boolean containsPoint(int x, int y) {
        return getBounds().contains(x, y);
    }

   
    
     /**
     * Nullifies the VisalisationTimer thread
     */
    public void stopVisualisation() {
        mVisuTimer.purge();
        mVisuTimer.cancel();

        // mVisuTimer = null;
    }
    
     /**
     *
     *
     */
    @Override
    public void update(java.util.Observable obs, Object obj) {
        update();
    }
    
    /**
     *
     *
     */
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
                    VisualisationTask visuTask = new VisualisationTask(mEditorConfig.sVISUALISATIONTIME, this);
                    mVisuTimer.schedule(visuTask, 0, 25);
                }
            }
        }
    }
    
    public Node getNode(){
        return mNode;
    }
}
