package de.dfki.vsm.editor.project.sceneflow.attributes;

//~--- non-JDK imports --------------------------------------------------------
import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.dialog.CmdDialog;
import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.dialog.ModifyCEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyIEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyPEdgeDialog;
import de.dfki.vsm.editor.dialog.ModifyTEdgeDialog;
import de.dfki.vsm.editor.dialog.TypeDefDialog;
import de.dfki.vsm.editor.dialog.VarDefDialog;
import de.dfki.vsm.editor.event.EdgeSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.model.sceneflow.Edge.Type;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.LogicalCond;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.RegularExpressions;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import java.util.HashMap;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;

import static java.awt.Component.RIGHT_ALIGNMENT;

///**
// * @author Gregor Mehlmann
// */
//abstract class AttributeEditor extends JPanel implements EventListener {
//
//    // The maintained data model node
//    protected Node mDataNode;
//
//    // GUI Components
//    protected final DefaultListModel mListModel;
//    protected final JList mList;
//
//    //
//    private final JPanel mEditorPanel;
//    private final JScrollPane mScrollPane;
//    private final JPanel mButtonPanel;
//    private final JButton mAddButton;
//    private final JButton mRemoveButton;
//    private final JButton mEditButton;
//    private final JButton mUpButton;
//    private final JButton mDownButton;
//
//    public AttributeEditor(String title) {
//
//        // Init the attribute list
//        mListModel = new DefaultListModel();
//        mList = new JList(mListModel);
//        mList.setCellRenderer(new StripedCellRenderer());
//        mScrollPane = new JScrollPane(mList);
//        mScrollPane.setMaximumSize(new Dimension(1000, 100));
//        mScrollPane.setPreferredSize(new Dimension(200, 100));
//        mScrollPane.setMinimumSize(new Dimension(200, 100));
//
//        // Init the button panel
//        mAddButton = new JButton(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add.png"));
//        mAddButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/add_blue.png"));
//        mAddButton.setMaximumSize(new Dimension(22, 22));
//        mAddButton.setPreferredSize(new Dimension(22, 22));
//        mAddButton.setMinimumSize(new Dimension(22, 22));
//        mAddButton.setOpaque(false);
//        mAddButton.setContentAreaFilled(false);
//        mAddButton.setFocusable(false);
//        mAddButton.setBorder(BorderFactory.createEmptyBorder());
//        mAddButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                add();
//                EditorInstance.getInstance().refresh();
//            }
//        });
//
//        //
//        mRemoveButton = new JButton(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/remove.png"));
//        mRemoveButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/remove_blue.png"));
//        mRemoveButton.setMinimumSize(new Dimension(22, 22));
//        mRemoveButton.setMaximumSize(new Dimension(22, 22));
//        mRemoveButton.setPreferredSize(new Dimension(22, 22));
//        mRemoveButton.setOpaque(false);
//        mRemoveButton.setContentAreaFilled(false);
//        mRemoveButton.setFocusable(false);
//        mRemoveButton.setBorder(BorderFactory.createEmptyBorder());
//        mRemoveButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                remove();
//                EditorInstance.getInstance().refresh();
//            }
//        });
//
//        //
//        mEditButton = new JButton(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/edit.png"));
//        mEditButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/edit_blue.png"));
//        mEditButton.setMinimumSize(new Dimension(22, 22));
//        mEditButton.setMaximumSize(new Dimension(22, 22));
//        mEditButton.setPreferredSize(new Dimension(22, 22));
//        mEditButton.setOpaque(false);
//        mEditButton.setContentAreaFilled(false);
//        mEditButton.setFocusable(false);
//        mEditButton.setBorder(BorderFactory.createEmptyBorder());
//        mEditButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                edit();
//                EditorInstance.getInstance().refresh();
//            }
//        });
//
//        //
//        mUpButton = new JButton(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up_20.png"));
//        mUpButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/up_20_blue.png"));
//        mUpButton.setMinimumSize(new Dimension(20, 20));
//        mUpButton.setMaximumSize(new Dimension(20, 20));
//        mUpButton.setPreferredSize(new Dimension(20, 20));
//        mUpButton.setOpaque(false);
//        mUpButton.setContentAreaFilled(false);
//        mUpButton.setFocusable(false);
//        mUpButton.setBorder(BorderFactory.createEmptyBorder());
//        mUpButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                up();
//                EditorInstance.getInstance().refresh();
//            }
//        });
//
//        //
//        mDownButton = new JButton(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/down_20.png"));
//        mDownButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/down_20_blue.png"));
//        mDownButton.setMinimumSize(new Dimension(22, 22));
//        mDownButton.setMaximumSize(new Dimension(22, 22));
//        mDownButton.setPreferredSize(new Dimension(22, 22));
//        mDownButton.setOpaque(false);
//        mDownButton.setContentAreaFilled(false);
//        mDownButton.setFocusable(false);
//        mDownButton.setBorder(BorderFactory.createEmptyBorder());
//        mDownButton.addActionListener(new ActionListener() {
//            @Override
//            public void actionPerformed(ActionEvent e) {
//                down();
//                EditorInstance.getInstance().refresh();
//            }
//        });
//
//        //
//        mButtonPanel = new JPanel();
//        mButtonPanel.setOpaque(false);
//        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.Y_AXIS));
//        mButtonPanel.setBorder(BorderFactory.createEmptyBorder());
//        mButtonPanel.add(mAddButton);
//        mButtonPanel.add(mRemoveButton);
//        mButtonPanel.add(mEditButton);
//        mButtonPanel.add(mUpButton);
//        mButtonPanel.add(mDownButton);
//
//        // Init the editor panel
//        mEditorPanel = new JPanel();
//        mEditorPanel.setOpaque(false);
//        mEditorPanel.setLayout(new BoxLayout(mEditorPanel, BoxLayout.X_AXIS));
//        mEditorPanel.add(mScrollPane);
//        mEditorPanel.add(mButtonPanel);
//
//        // Init the attribute editor
//        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
//        setOpaque(false);
//        setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), title));
//        add(mEditorPanel);
//        add(Box.createRigidArea(new Dimension(20, 100)));
//
//        // Add the attribute editor to the event multicaster
//        EventDispatcher.getInstance().register(this);
//    }
//
//    protected abstract void add();
//
//    protected abstract void edit();
//
//    protected abstract void remove();
//
//    protected abstract void up();
//
//    protected abstract void down();
//
//    private class StripedCellRenderer extends JLabel implements ListCellRenderer {
//
//        public StripedCellRenderer() {
//            setOpaque(true);
//        }
//
//        @Override
//        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
//                boolean cellHasFocus) {
//            setText(value.toString());
//
//            Color background;
//            Color foreground;
//
//            // check if this cell represents the current DnD drop location
//            JList.DropLocation dropLocation = list.getDropLocation();
//
//            if ((dropLocation != null) && !dropLocation.isInsert() && (dropLocation.getIndex() == index)) {
//                background = Color.BLUE;
//                foreground = Color.WHITE;
//
//                // check if this cell is selected
//            } else if (isSelected) {
//
//                // background = Color.ORANGE;
//                background = new Color(25, 33, 243, 200);
//                foreground = Color.WHITE;
//
//                // unselected, and not the DnD drop location
//            } else {
//                if (index % 2 == 0) {
//
//                    // background = new Color(255, 240, 240);
//                    background = Color.WHITE;
//                    foreground = Color.BLACK;
//                } else {
//                    background = new Color(235, 235, 235, 127);
//
//                    // background = new Color(240, 240, 255);
//                    foreground = Color.BLACK;
//                }
//            }
//
//            setBackground(background);
//            setForeground(foreground);
//
//            return this;
//        }
//    }
//}
/**
 *
 *
 * @author Not me
 *
 *
 */
class CmdEditor extends AttributeEditor
{

    public CmdEditor()
    {
        super("Edit Command Executions:");
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the command execution list
            mListModel.clear();

            for (Command cmd : mDataNode.getCmdList())
            {
                mListModel.addElement(cmd);
            }
        }
        else
        {

            // Do nothing
        }
    }

    @Override
    protected void add()
    {
        de.dfki.vsm.editor.Node currentNode = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().getNode(mDataNode.getId());
        if (currentNode != null)
        {
            EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().deselectAllOtherComponents(currentNode);
            Command cmd = new CmdDialog(null).run();

            if (cmd != null)
            {
                mDataNode.addCmd(cmd);
                mListModel.addElement(cmd);
            }
        }
    }

    @Override
    protected void edit()
    {
        int index = mList.getSelectedIndex();

        if (index >= 0)
        {
            Command oldCmd = mDataNode.getCmdAt(index);
            Command newCmd = new CmdDialog(oldCmd).run();

            if (newCmd != null)
            {
                mDataNode.setCmdAt(newCmd, index);
                mListModel.set(index, newCmd);
            }
        }
    }

    @Override
    protected void remove()
    {
        int index = mList.getSelectedIndex();

        if (index != -1)
        {
            mDataNode.removeCmdAt(index);
            mListModel.removeElementAt(index);
        }
    }

    @Override
    protected void up()
    {
        int index = mList.getSelectedIndex();

        if (index >= 1)
        {
            Command thisCmd = mDataNode.getCmdAt(index);
            Command otherCmd = mDataNode.getCmdAt(index - 1);

            mDataNode.setCmdAt(thisCmd, index - 1);
            mDataNode.setCmdAt(otherCmd, index);
            mList.setSelectedIndex(index - 1);
            mListModel.set(index - 1, thisCmd);
            mListModel.set(index, otherCmd);
        }
    }

    @Override
    protected void down()
    {
        int index = mList.getSelectedIndex();

        if ((index >= 0) && (index < mListModel.size() - 1))
        {
            Command thisCmd = mDataNode.getCmdAt(index);
            Command otherCmd = mDataNode.getCmdAt(index + 1);

            mDataNode.setCmdAt(thisCmd, index + 1);
            mDataNode.setCmdAt(otherCmd, index);
            mList.setSelectedIndex(index + 1);
            mListModel.set(index + 1, thisCmd);
            mListModel.set(index, otherCmd);
        }
    }
}

/**
 *
 *
 * @author Sergio Soto
 *
 *
 */
class ConditionEditor extends JPanel implements EventListener
{

    private CEdge mDataCEdge;
    private ModifyCEdgeDialog mCEdgeDialog;

    public ConditionEditor()
    {
        initComponents();
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {
        setBackground(Color.white);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof EdgeSelectedEvent)
        {
            if (event instanceof EdgeSelectedEvent)
            {
                if (((EdgeSelectedEvent) event).getEdge().getEdgeType().equals(Type.CEdge))
                {
                    mDataCEdge = (CEdge) ((EdgeSelectedEvent) event).getEdge();
                    mCEdgeDialog = new ModifyCEdgeDialog(mDataCEdge);
                    removeAll();
                    mCEdgeDialog.getInputPanel().setMinimumSize(new Dimension(200, 40));
                    mCEdgeDialog.getInputPanel().setMaximumSize(new Dimension(1000, 40));
                    mCEdgeDialog.getInputPanel().setPreferredSize(new Dimension(200, 40));
                    mCEdgeDialog.getInputPanel().setAlignmentX(RIGHT_ALIGNMENT);
                    mCEdgeDialog.getAltStartNodePanel().setMinimumSize(new Dimension(200, 150));
                    mCEdgeDialog.getAltStartNodePanel().setMaximumSize(new Dimension(1000, 150));
                    mCEdgeDialog.getAltStartNodePanel().setPreferredSize(new Dimension(200, 150));
                    mCEdgeDialog.getAltStartNodePanel().setAlignmentX(RIGHT_ALIGNMENT);
                    add(mCEdgeDialog.getInputPanel());
                    add(mCEdgeDialog.getAltStartNodePanel());
                    mCEdgeDialog.getInputTextField().addKeyListener(new KeyAdapter()
                    {
                        @Override
                        public void keyReleased(KeyEvent event)
                        {
                            save();
                            EditorInstance.getInstance().refresh();
                        }
                    });
                }
            }
        }
        else
        {

            // Do nothing
        }
    }

    private void save()
    {
        String inputString = mCEdgeDialog.getInputTextField().getText().trim();

        try
        {
            _SFSLParser_.parseResultType = _SFSLParser_.LOG;
            _SFSLParser_.run(inputString);

            LogicalCond log = _SFSLParser_.logResult;

            if ((log != null) && !_SFSLParser_.errorFlag)
            {
                mDataCEdge.setCondition(log);
            }
            else
            {

                // Do nothing
            }
        } catch (Exception e)
        {
        }
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class EdgeEditor extends JPanel implements EventListener
{

    private final TimeOutEditor mTimeOutEditor;
    private final ConditionEditor mConditionEditor;
    private final ProbabilityEditor mProbabilityEditor;
    private final InterruptEditor mInterruptEditor;

    public EdgeEditor()
    {

        // Init the child editors
        mTimeOutEditor = new TimeOutEditor();
        mConditionEditor = new ConditionEditor();
        mProbabilityEditor = new ProbabilityEditor();
        mInterruptEditor = new InterruptEditor();

        // Init components
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBackground(Color.white);
        setBorder(BorderFactory.createEmptyBorder());
        add(mTimeOutEditor);
        add(mConditionEditor);
        add(mProbabilityEditor);
        add(mInterruptEditor);
        EventDispatcher.getInstance().register(this);
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof EdgeSelectedEvent)
        {

            // Get the selected node
            Edge edge = ((EdgeSelectedEvent) event).getEdge();

            if (edge instanceof TEdge)
            {
                mTimeOutEditor.setVisible(true);
            }
            else
            {
                mTimeOutEditor.setVisible(false);
            }

            if (edge instanceof CEdge)
            {
                mConditionEditor.setVisible(true);
            }
            else
            {
                mConditionEditor.setVisible(false);
            }

            if (edge instanceof IEdge)
            {
                mInterruptEditor.setVisible(true);
            }
            else
            {
                mInterruptEditor.setVisible(false);
            }

            if (edge instanceof PEdge)
            {
                mProbabilityEditor.setVisible(true);
            }
            else
            {
                mProbabilityEditor.setVisible(false);
            }
        }
        else
        {

            // Do nothing
        }
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
public class ElementEditor extends JScrollPane implements EventListener
{

    //
    // private final Observable mObservable = new Observable();
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    private final NodeEditor mNodeEditor;
    private final EdgeEditor mEdgeEditor;

    public ElementEditor()
    {

        // Init node editor and edge editor
        mNodeEditor = new NodeEditor();
        mEdgeEditor = new EdgeEditor();

        //
        // Init the scrollpane attributes
        setPreferredSize(new Dimension(260, 500));
        setMinimumSize(new Dimension(260, 500));
        setBorder(BorderFactory.createEtchedBorder());
        setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        getViewport().setOpaque(false);
        setOpaque(false);

        // Set the initial viewport to null
        setViewportView(null);

        // Add the element editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    public final void refresh()
    {

        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the node of the node editor
            // mNodeEditor.update(((NodeSelectedEvent) event).getNode());
            setViewportView(mNodeEditor);
        }
        else if (event instanceof EdgeSelectedEvent)
        {

            // Update the edge of the edge editor
            // mEdgeEditor.update(((EdgeSelectedEvent) event).getEdge());
            setViewportView(mEdgeEditor);
        }
        else
        {

            // Do nothing
        }
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class FunDefEditor extends AttributeEditor
{

    public FunDefEditor()
    {
        super("Edit Function Definitions:");
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the function definition map
            if (mDataNode instanceof SceneFlow)
            {
                mListModel.clear();

                for (FunDef def : ((SceneFlow) mDataNode).getUsrCmdDefMap().values())
                {
                    mListModel.addElement(def);
                }
            }
        }
        else
        {

            // Do nothing
        }
    }

    @Override
    protected void add()
    {
        FunDef usrCmdDef = new FunDefDialog(null).run();

        if (usrCmdDef != null)
        {
            ((SceneFlow) mDataNode).putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
            mListModel.addElement(usrCmdDef);
        }
    }

    @Override
    protected void edit()
    {
        int index = mList.getSelectedIndex();

        if (index >= 0)
        {
            FunDef oldUsrCmdDef = (FunDef) mList.getSelectedValue();

            // Remove the old function definition from the sceneflow
            ((SceneFlow) mDataNode).removeUsrCmdDef(oldUsrCmdDef.getName());

            // Edit the old function definition
            FunDef newUsrCmdDef = new FunDefDialog(oldUsrCmdDef).run();

            if (newUsrCmdDef != null)
            {

                // Put the new function definition to the sceneflow
                ((SceneFlow) mDataNode).putUsrCmdDef(newUsrCmdDef.getName(), newUsrCmdDef);

                //
                mListModel.set(index, newUsrCmdDef);
            }
        }
    }

    @Override
    protected void remove()
    {
        FunDef oldUsrCmdDef = (FunDef) mList.getSelectedValue();

        if (oldUsrCmdDef != null)
        {

            // Remove the old function definition from the sceneflow
            ((SceneFlow) mDataNode).removeUsrCmdDef(oldUsrCmdDef.getName());

            //
            mListModel.removeElement(oldUsrCmdDef);
        }
    }

    @Override

    protected void up()
    {
    }

    @Override
    protected void down()
    {
    }
}

/**
 *
 *
 * @author Sergio Soto
 *
 *
 */
class InterruptEditor extends JPanel implements EventListener
{

    private IEdge mDataIEdge;
    private ModifyIEdgeDialog mIEdgeDialog;

    public InterruptEditor()
    {
        initComponents();
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {
        setBackground(Color.white);
        setPreferredSize(new Dimension(500, 270));
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof EdgeSelectedEvent)
        {
            if (event instanceof EdgeSelectedEvent)
            {
                if (((EdgeSelectedEvent) event).getEdge().getEdgeType().equals(Type.IEdge))
                {
                    mDataIEdge = (IEdge) ((EdgeSelectedEvent) event).getEdge();
                    mIEdgeDialog = new ModifyIEdgeDialog(mDataIEdge);
                    removeAll();
                    mIEdgeDialog.getInputPanel().setMinimumSize(new Dimension(200, 40));
                    mIEdgeDialog.getInputPanel().setMaximumSize(new Dimension(1000, 40));
                    mIEdgeDialog.getInputPanel().setPreferredSize(new Dimension(200, 40));
                    mIEdgeDialog.getInputPanel().setAlignmentX(RIGHT_ALIGNMENT);
                    mIEdgeDialog.getAltStartNodePanel().setMinimumSize(new Dimension(200, 150));
                    mIEdgeDialog.getAltStartNodePanel().setMaximumSize(new Dimension(1000, 150));
                    mIEdgeDialog.getAltStartNodePanel().setPreferredSize(new Dimension(200, 150));
                    mIEdgeDialog.getAltStartNodePanel().setAlignmentX(RIGHT_ALIGNMENT);
                    add(mIEdgeDialog.getInputPanel());
                    add(mIEdgeDialog.getAltStartNodePanel());
                    add(Box.createVerticalGlue());
                    mIEdgeDialog.getInputTextField().addKeyListener(new KeyAdapter()
                    {
                        @Override
                        public void keyReleased(KeyEvent event)
                        {
                            save();
                            EditorInstance.getInstance().refresh();
                        }
                    });
                }
            }
        }
        else
        {

            // Do nothing
        }
    }

    private void save()
    {
        String inputString = mIEdgeDialog.getInputTextField().getText().trim();

        try
        {
            _SFSLParser_.parseResultType = _SFSLParser_.LOG;
            _SFSLParser_.run(inputString);

            LogicalCond log = _SFSLParser_.logResult;

            if ((log != null) && !_SFSLParser_.errorFlag)
            {
                mDataIEdge.setCondition(log);
            }
            else
            {

                // Do nothing
            }
        } catch (Exception e)
        {
        }
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class NameEditor extends JPanel implements EventListener
{

    private JTextField mNameField;
    private Node mDataNode;

    public NameEditor()
    {
        initComponents();
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {

        // Init the node name text field
        mNameField = new JTextField();
        mNameField.setMinimumSize(new Dimension(200, 20));
        mNameField.setMaximumSize(new Dimension(1000, 20));
        mNameField.setPreferredSize(new Dimension(200, 20));
        mNameField.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyReleased(KeyEvent event)
            {
                save();
                EditorInstance.getInstance().refresh();
            }
        });

        // Init the node name panel
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setOpaque(false);
        setBorder(BorderFactory.createTitledBorder(BorderFactory.createEmptyBorder(), "Edit Node Name:"));
        add(mNameField);
        add(Box.createRigidArea(new Dimension(40, 20)));
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the node name
            mNameField.setText(mDataNode.getName());
        }
        else
        {

            // Do nothing
        }
    }

    private void save()
    {

        mDataNode.setName(sanitizeString(mNameField.getText().trim()));
    }

    //ESCAPES STRINGS
    private String sanitizeString(String st)
    {
        String output = st;
        output = output.replaceAll("'", "");
        output = output.replaceAll("\"", "");
        return output;
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class NodeEditor extends JPanel implements EventListener
{

    private final NameEditor mNameEditor;
    private final StartNodeEditor mStartNodeEditor;
    private final TypeDefEditor mTypeDefEditor;
    private final VarDefEditor mVarDefEditor;

    // private final FunDefEditor mFunDefEditor;
    private final CmdEditor mCmdEditor;

    public NodeEditor()
    {

        // Init the child editors
        mNameEditor = new NameEditor();
        mStartNodeEditor = new StartNodeEditor();
        mTypeDefEditor = new TypeDefEditor();

        // mFunDefEditor = new FunDefEditor();
        mVarDefEditor = new VarDefEditor();
        mCmdEditor = new CmdEditor();

        // Init components
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBackground(Color.white);
        setBorder(BorderFactory.createEmptyBorder());
        add(mNameEditor);
        add(mStartNodeEditor);
        add(mTypeDefEditor);
        add(mVarDefEditor);

        // add(mFunDefEditor);
        add(mCmdEditor);

        // Add the element editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Get the selected node
            Node node = ((NodeSelectedEvent) event).getNode();

            // Show or hide the start node editor
            if (node instanceof SuperNode)
            {
                mStartNodeEditor.setVisible(true);
            }
            else
            {
                mStartNodeEditor.setVisible(false);
            }

            // Show or hide the function definition editor
            if (node instanceof SceneFlow)
            {

                // mFunDefEditor.setVisible(true);
            }
            else
            {

                // mFunDefEditor.setVisible(false);
            }
        }
        else
        {

            // Do nothing
        }
    }
}

/**
 *
 *
 * @author Sergio Soto
 *
 *
 */
class ProbabilityEditor extends JPanel implements EventListener
{

    private final HashMap<PEdge, JTextField> mPEdgeMap = new HashMap<PEdge, JTextField>();
    private PEdge mDataPEdge;
    private ModifyPEdgeDialog mPEdgeDialog;
    private JPanel mButtonPanel;

    public ProbabilityEditor()
    {
        initComponents();
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {
        setBackground(Color.white);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setAlignmentY(CENTER_ALIGNMENT);
        setAlignmentX(CENTER_ALIGNMENT);
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof EdgeSelectedEvent)
        {
            if (((EdgeSelectedEvent) event).getEdge().getEdgeType().equals(Type.PEdge))
            {
                mDataPEdge = (PEdge) ((EdgeSelectedEvent) event).getEdge();
                mPEdgeDialog = new ModifyPEdgeDialog(mDataPEdge);
                removeAll();
                mPEdgeDialog.getEdgeProbPanel().setMinimumSize(new Dimension(200, 140));
                mPEdgeDialog.getEdgeProbPanel().setMaximumSize(new Dimension(1000, 140));
                mPEdgeDialog.getEdgeProbPanel().setPreferredSize(new Dimension(200, 140));
                add(mPEdgeDialog.getEdgeProbPanel());
                mPEdgeDialog.getAltStartNodePanel().setMinimumSize(new Dimension(200, 150));
                mPEdgeDialog.getAltStartNodePanel().setMaximumSize(new Dimension(1000, 150));
                mPEdgeDialog.getAltStartNodePanel().setPreferredSize(new Dimension(200, 150));
                add(mPEdgeDialog.getAltStartNodePanel());

                for (JTextField textField : mPEdgeDialog.getPEdgeMap().values())
                {
                    textField.addKeyListener(new KeyAdapter()
                    {
                        @Override
                        public void keyReleased(KeyEvent event)
                        {
                            save();
                        }
                    });
                }

                mPEdgeDialog.getNormButton().addMouseListener(new java.awt.event.MouseAdapter()
                {
                    public void mouseClicked(java.awt.event.MouseEvent evt)
                    {
                        mPEdgeDialog.normalizeActionPerformed();
                        save();
                    }
                });
                mPEdgeDialog.getUniButton().addMouseListener(new java.awt.event.MouseAdapter()
                {
                    public void mouseClicked(java.awt.event.MouseEvent evt)
                    {
                        mPEdgeDialog.uniformActionPerformed();
                        save();
                    }
                });
                mButtonPanel = new JPanel();
                mButtonPanel.setOpaque(false);
                mButtonPanel.setMinimumSize(new Dimension(440, 40));
                mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
                mButtonPanel.add(Box.createRigidArea(new Dimension(20, 10)));
                mButtonPanel.add(mPEdgeDialog.getUniButton());
                mButtonPanel.add(Box.createRigidArea(new Dimension(20, 10)));
                mButtonPanel.add(mPEdgeDialog.getNormButton());
                add(Box.createRigidArea(new Dimension(20, 20)));
                add(mButtonPanel);
            }
        }
        else
        {

            // Do nothing
        }
    }

    private void save()
    {
        mPEdgeDialog.okActionPerformed();
        EditorInstance.getInstance().refresh();

        // System.out.println("save");
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class StartNodeEditor extends AttributeEditor
{

    public StartNodeEditor()
    {
        super("Edit Start Nodes:");
        disableAddButton();
        disableUpDownButtons();
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the start node list
            if (mDataNode instanceof SuperNode)
            {
                mListModel.clear();

                for (Node startNode : ((SuperNode) mDataNode).getStartNodeMap().values())
                {
                    mListModel.addElement(startNode.getName() + "(" + startNode.getId() + ")");
                }
            }
        }
        else
        {

            // Do nothing
        }
    }

    @Override
    protected void add()
    {

        // Create list of child nodes
        Vector<String> nodeDataList = new Vector<>();

        for (Node node : ((SuperNode) mDataNode).getNodeAndSuperNodeList())
        {
            if (!node.isHistoryNode())
            {
                nodeDataList.add(node.getName() + "(" + node.getId() + ")");
            }
        }
    }

    @Override
    protected void remove()
    {
        String value = (String) mList.getSelectedValue();

        if (value != null)
        {
            String id = RegularExpressions.getMatches(value, "\\((\\w*)\\)", 2).get(1);

            // Get the new start node
            Node oldStartNode = ((SuperNode) mDataNode).getChildNodeById(id);

            ((SuperNode) mDataNode).removeStartNode(oldStartNode);
            EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getWorkSpace().getNode(id).removeStartSign();
            mListModel.removeElement(value);
            EditorInstance.getInstance().refresh();
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();
        }

        // Reload the current start node list of the supernode
    }

    @Override
    protected void edit()
    {
    }

    @Override
    protected void up()
    {
    }

    @Override
    protected void down()
    {
    }
}

/**
 *
 *
 * @author Sergio Soto
 *
 *
 */
class TimeOutEditor extends JPanel implements EventListener
{

    private TEdge mDataTEdge;
    private ModifyTEdgeDialog mTEdgeDialog;

    public TimeOutEditor()
    {
        initComponents();
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {
        setBackground(Color.white);
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setAlignmentX(RIGHT_ALIGNMENT);
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof EdgeSelectedEvent)
        {
            if (((EdgeSelectedEvent) event).getEdge().getEdgeType().equals(Type.TEdge))
            {
                mDataTEdge = (TEdge) ((EdgeSelectedEvent) event).getEdge();
                mTEdgeDialog = new ModifyTEdgeDialog(mDataTEdge);
                removeAll();
                mTEdgeDialog.getInputPanel().setMinimumSize(new Dimension(200, 40));
                mTEdgeDialog.getInputPanel().setMaximumSize(new Dimension(1000, 40));
                mTEdgeDialog.getInputPanel().setPreferredSize(new Dimension(200, 40));
                mTEdgeDialog.getInputPanel().setAlignmentX(RIGHT_ALIGNMENT);
                mTEdgeDialog.getAltStartNodePanel().setMinimumSize(new Dimension(200, 150));
                mTEdgeDialog.getAltStartNodePanel().setMaximumSize(new Dimension(1000, 150));
                mTEdgeDialog.getAltStartNodePanel().setPreferredSize(new Dimension(200, 150));
                mTEdgeDialog.getAltStartNodePanel().setAlignmentX(RIGHT_ALIGNMENT);
                add(mTEdgeDialog.getInputPanel());
                add(mTEdgeDialog.getAltStartNodePanel());
                mTEdgeDialog.getInputTextField().addKeyListener(new KeyAdapter()
                {
                    @Override
                    public void keyReleased(KeyEvent event)
                    {
                        save();
                        EditorInstance.getInstance().refresh();
                    }
                });
            }
        }
        else
        {

            // Do nothing
        }
    }

    private void save()
    {
        mDataTEdge.setTimeout(Integer.parseInt(mTEdgeDialog.getInputTextField().getText()));
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class TypeDefEditor extends AttributeEditor
{

    public TypeDefEditor()
    {
        super("Edit Type Definitions:");
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the type definition list
            mListModel.clear();

            for (TypeDef def : mDataNode.getTypeDefList())
            {
                mListModel.addElement(def);
            }
        }
    }

    @Override
    protected void add()
    {
        TypeDef typeDef = new TypeDefDialog(null).run();

        if (typeDef != null)
        {
            mDataNode.addTypeDef(typeDef);
            mListModel.addElement(typeDef);
        }
    }

    @Override
    protected void edit()
    {
        int index = mList.getSelectedIndex();

        if (index >= 0)
        {
            TypeDef oldTypeDef = mDataNode.getTypeDefAt(index);
            TypeDef newTypeDef = new TypeDefDialog(oldTypeDef).run();

            //
            if (newTypeDef != null)
            {
                mDataNode.setTypeDefAt(newTypeDef, index);
                mListModel.set(index, newTypeDef);
            }
        }
    }

    @Override
    protected void remove()
    {
        int index = mList.getSelectedIndex();

        if (index != -1)
        {
            mDataNode.removeTypeDefAt(index);
            mListModel.removeElementAt(index);
        }
    }

    @Override
    protected void up()
    {
        int index = mList.getSelectedIndex();

        if (index >= 1)
        {
            TypeDef thisTypeDef = mDataNode.getTypeDefAt(index);
            TypeDef otherTypeDef = mDataNode.getTypeDefAt(index - 1);

            mDataNode.setTypeDefAt(thisTypeDef, index - 1);
            mDataNode.setTypeDefAt(otherTypeDef, index);
            mList.setSelectedIndex(index - 1);
            mListModel.set(index - 1, thisTypeDef);
            mListModel.set(index, otherTypeDef);
        }
    }

    @Override
    protected void down()
    {
        int index = mList.getSelectedIndex();

        if ((index >= 0) && (index < mListModel.size() - 1))
        {
            TypeDef thisTypeDef = mDataNode.getTypeDefAt(index);
            TypeDef otherTypeDef = mDataNode.getTypeDefAt(index + 1);

            mDataNode.setTypeDefAt(thisTypeDef, index + 1);
            mDataNode.setTypeDefAt(otherTypeDef, index);
            mList.setSelectedIndex(index + 1);
            mListModel.set(index + 1, thisTypeDef);
            mListModel.set(index, otherTypeDef);
        }
    }
}

/**
 *
 *
 * @author Not me
 *
 *
 */
class VarDefEditor extends AttributeEditor
{

    public VarDefEditor()
    {
        super("Edit Variable Definitions:");
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof NodeSelectedEvent)
        {

            // Update the selected node
            mDataNode = ((NodeSelectedEvent) event).getNode();

            // Reload the variable definition list
            mListModel.clear();

            for (VarDef def : mDataNode.getVarDefList())
            {
                mListModel.addElement(def);
            }
        }
        else
        {

            // Do nothing
        }
    }

    @Override
    public void add()
    {

        // Show the variable definition dialog
        VarDef varDef = new VarDefDialog(mDataNode, null).run();

        // Add the new variable definition if the creation was successful
        if (varDef != null)
        {
            mDataNode.addVarDef(varDef);
            mListModel.addElement(varDef);
        }
    }

    @Override
    public void edit()
    {
        int index = mList.getSelectedIndex();

        if (index > -1)
        {
            VarDef oldVarDef = mDataNode.getVarDefAt(index);
            VarDef newVarDef = new VarDefDialog(mDataNode, oldVarDef).run();

            // Add the new variable definition if the creation was successful
            if (newVarDef != null)
            {
                mDataNode.setVarDefAt(newVarDef, index);
                mListModel.set(index, newVarDef);
            }
        }
    }

    @Override
    public void remove()
    {
        int index = mList.getSelectedIndex();

        if (index != -1)
        {
            mDataNode.removeVarDefAt(index);
            mListModel.removeElementAt(index);
        }
    }

    @Override
    public void up()
    {
        int index = mList.getSelectedIndex();

        if (index >= 1)
        {
            VarDef thisVarDef = mDataNode.getVarDefAt(index);
            VarDef otherVarDef = mDataNode.getVarDefAt(index - 1);

            mDataNode.setVarDefAt(thisVarDef, index - 1);
            mDataNode.setVarDefAt(otherVarDef, index);
            mList.setSelectedIndex(index - 1);
            mListModel.set(index - 1, thisVarDef);
            mListModel.set(index, otherVarDef);
        }
    }

    @Override
    public void down()
    {
        int index = mList.getSelectedIndex();

        if ((index >= 0) && (index < mListModel.size() - 1))
        {
            VarDef thisVarDef = mDataNode.getVarDefAt(index);
            VarDef otherVarDef = mDataNode.getVarDefAt(index + 1);

            mDataNode.setVarDefAt(thisVarDef, index + 1);
            mDataNode.setVarDefAt(otherVarDef, index);
            mList.setSelectedIndex(index + 1);
            mListModel.set(index + 1, thisVarDef);
            mListModel.set(index, otherVarDef);
        }
    }
}
