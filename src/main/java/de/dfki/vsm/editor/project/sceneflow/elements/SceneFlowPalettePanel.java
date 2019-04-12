package de.dfki.vsm.editor.project.sceneflow.elements;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.PreferencesDesktop;
import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.util.log.LOGDefaultLogger;

import javax.swing.*;
import java.awt.*;

//~--- JDK imports ------------------------------------------------------------

/**
 * @author Sergio Soto
 * @author Gregor Mehlmann
 */
public class SceneFlowPalettePanel extends JPanel {

    private final SceneFlowPaletteItem mSuperNodeEntry = 
            new SceneFlowPaletteItem("Super Node", "Holds Sub-Scences flow",
                    PreferencesDesktop.ICON_SUPERNODE_STANDARD, PreferencesDesktop.ICON_SUPERNODE_ROLLOVER, PreferencesDesktop.ICON_SUPERNODE_DRAGGING,
                    Node.Type.SuperNode);
    
    private final SceneFlowPaletteItem mBasicNodeEntry = 
            new SceneFlowPaletteItem("Basic Node", "Holds Scenes Actions",
                    PreferencesDesktop.ICON_BASICNODE_STANDARD, PreferencesDesktop.ICON_BASICNODE_ROLLOVER, PreferencesDesktop.ICON_BASICNODE_DRAGGING,
                    Node.Type.BasicNode);
    
    private final SceneFlowPaletteItem mEEdgeEntry = 
            new SceneFlowPaletteItem("Epsilon Edge", "Creates Epsilon Transition",
                    PreferencesDesktop.ICON_EEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_EEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_EEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.EEDGE));
    
    private final SceneFlowPaletteItem mTEdgeEntry = 
            new SceneFlowPaletteItem("Timeout Edge", "Creates Timeout Transition",
                    PreferencesDesktop.ICON_TEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_TEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_TEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.TEDGE));
    
    private final SceneFlowPaletteItem mPEdgeEntry = 
            new SceneFlowPaletteItem("Probability Edge", "Creates Probability Transition",
                    PreferencesDesktop.ICON_PEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_PEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_PEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.PEDGE));
    
    private final SceneFlowPaletteItem mCEdgeEntry = 
            new SceneFlowPaletteItem("Conditional Edge", "Creates Conditional Transition",
                    PreferencesDesktop.ICON_CEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_CEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_CEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.CEDGE));
    
    private final SceneFlowPaletteItem mIEdgeEntry = 
            new SceneFlowPaletteItem("Interruptive Edge", "Creates Interrutive Transition",
                    PreferencesDesktop.ICON_IEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_IEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_IEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.IEDGE));
    
    private final SceneFlowPaletteItem mFEdgeEntry = 
            new SceneFlowPaletteItem("Fork Edge", "Creates Forked Transition",
                    PreferencesDesktop.ICON_FEDGE_ENTRY_STANDARD, PreferencesDesktop.ICON_FEDGE_ENTRY_ROLLOVER, PreferencesDesktop.ICON_FEDGE_ENTRY_DRAGGING,
                    new Edge(Edge.TYPE.FEDGE));
    
    private final SceneFlowPaletteItem mCommentEntry = 
            new SceneFlowPaletteItem("Comment", "Adds a Comment",
                    PreferencesDesktop.ICON_COMMENT_ENTRY_STANDARD, PreferencesDesktop.ICON_COMMENT_ENTRY_ROLLOVER, PreferencesDesktop.ICON_COMMENT_ENTRY_DRAGGING,
                    new Comment());
    
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    
    //
    private final int paletteDimension = 230;
    // Construct the tool panel
    public SceneFlowPalettePanel() {
        setLayout(new GridLayout(0, 3));
        setBackground(Color.WHITE);
        setPreferredSize(new Dimension(paletteDimension, paletteDimension));
        setMinimumSize(new Dimension(paletteDimension, paletteDimension));
        setMaximumSize(new Dimension(paletteDimension, paletteDimension));
        setBorder(BorderFactory.createEtchedBorder());
        add(mSuperNodeEntry);
        add(mBasicNodeEntry);
        add(mCommentEntry);
        add(mEEdgeEntry);
        add(mPEdgeEntry);
        add(mFEdgeEntry);
        add(mCEdgeEntry);
        add(mTEdgeEntry);
        add(mIEdgeEntry);
        ToolTipManager.sharedInstance().registerComponent(this);
    }
    
    public final void refresh() {
        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
        //
    }
}
