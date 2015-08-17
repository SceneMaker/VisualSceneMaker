package de.dfki.vsm.editor.project.sceneflow.elements;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.Comment;
import de.dfki.vsm.editor.Edge;
import de.dfki.vsm.editor.Node;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.ToolTipManager;

/**
 * @author Sergio Soto
 * @author Gregor Mehlmann
 */
public class SceneFlowPalettePanel extends JPanel {

    private final SceneFlowPaletteItem mSuperNodeEntry = 
            new SceneFlowPaletteItem("Super Node", "Holds Sub-Scences flow","SUPERNODE_ENTRY", Node.Type.SuperNode);
    private final SceneFlowPaletteItem mBasicNodeEntry = 
            new SceneFlowPaletteItem("Basic Node", "Holds Scenes Actions","BASICNODE_ENTRY", Node.Type.BasicNode);
    private final SceneFlowPaletteItem mEEdgeEntry = 
            new SceneFlowPaletteItem("Epsilon Edge", "Creates Epsilon Transition","EEDGE_ENTRY", new Edge(Edge.TYPE.EEDGE));
    private final SceneFlowPaletteItem mTEdgeEntry = 
            new SceneFlowPaletteItem("Timeout Edge", "Creates Timeout Transition","TEDGE_ENTRY", new Edge(Edge.TYPE.TEDGE));
    private final SceneFlowPaletteItem mPEdgeEntry = 
            new SceneFlowPaletteItem("Probability Edge", "Creates Probability Transition","PEDGE_ENTRY", new Edge(Edge.TYPE.PEDGE));
    private final SceneFlowPaletteItem mCEdgeEntry = 
            new SceneFlowPaletteItem("Conditional Edge", "Creates Conditional Transition","CEDGE_ENTRY", new Edge(Edge.TYPE.CEDGE));
    private final SceneFlowPaletteItem mIEdgeEntry = 
            new SceneFlowPaletteItem("Interruptive Edge", "Creates Interrutive Transition","IEDGE_ENTRY", new Edge(Edge.TYPE.IEDGE));
    private final SceneFlowPaletteItem mFEdgeEntry = 
            new SceneFlowPaletteItem("Fork Edge", "Creates Forked Transition", "FEDGE_ENTRY",new Edge(Edge.TYPE.FEDGE));
    private final SceneFlowPaletteItem mCommentEntry = 
            new SceneFlowPaletteItem("Comment", "Adds a Comment", "COMMENT_ENTRY",new Comment());
    
    //
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    
    // Construct the tool panel
    public SceneFlowPalettePanel() {
        setLayout(new GridLayout(0, 2));
        setBackground(Color.WHITE);
        setPreferredSize(new Dimension(250, 330));
        setMinimumSize(new Dimension(250, 330));
        setMaximumSize(new Dimension(250, 330));
        setBorder(BorderFactory.createEtchedBorder());
        add(mSuperNodeEntry);
        add(mBasicNodeEntry);
        add(mEEdgeEntry);
        add(mPEdgeEntry);
        add(mFEdgeEntry);
        add(mCEdgeEntry);
        add(mTEdgeEntry);
        add(mIEdgeEntry);
        add(mCommentEntry);
        //   
        ToolTipManager.sharedInstance().registerComponent(this);
    }
    
    public final void refresh() {
        // Print some information
        mLogger.message("Refreshing '" + this + "'");
        //
    }
}
