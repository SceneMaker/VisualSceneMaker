package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.Color;

//~--- JDK imports ------------------------------------------------------------

import java.util.Iterator;
import java.util.Map;

import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

/**
 * A dialog to create a conditional edge
 *
 * @author Gregor Mehlmann
 */
public class CreateIEdgeDialog extends Dialog {

    // The edge that should be created
    private final IEdge               mIEdge;
    private final AltStartNodeManager mAltStartNodeManager;

    // GUI-Components
    private JPanel       mInputPanel;
    private JLabel       mInputLabel;
    private JPanel       mButtonPanel;
    private JTextField   mInputTextField;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private JPanel       mAltStartNodePanel;
    private JLabel       mAltStartNodeLabel;
    private JList        mAltStartNodeList;
    private JScrollPane  mAltStartNodeScrollPane;
    private AddButton    mAddAltStartNodeButton;
    private RemoveButton mRemoveAltStartNodeButton;
    private EditButton   mEditAltStartNodeButton;

    public CreateIEdgeDialog(Node sourceNode, Node targetNode) {
        super(Editor.getInstance(), "Create Interruptive Edge", true);

        // Init edge data
        mIEdge = new IEdge();
        mIEdge.setTarget(targetNode.getId());
        mIEdge.setSourceNode(sourceNode);
        mIEdge.setTargetNode(targetNode);

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mIEdge);

        // Init the GUI-Components
        initComponents();
    }

    private void initComponents() {

        // Init input panel
        initInputPanel();

        // Init button panel
        initButtonPanel();

        // Init alternative start node panel
        initAltStartNodePanel();

        //
        addComponent(mInputLabel, 10, 10, 100, 30);
        addComponent(mInputTextField, 120, 10, 230, 30);

        //
        addComponent(mAltStartNodeLabel, 10, 75, 100, 30);
        addComponent(mAltStartNodeScrollPane, 120, 75, 230, 110);
        addComponent(mAddAltStartNodeButton, 355, 85, 20, 20);
        addComponent(mRemoveAltStartNodeButton, 355, 115, 20, 20);
        addComponent(mEditAltStartNodeButton, 355, 145, 20, 20);

        //
        addComponent(mCancelButton, 75, 210, 125, 30);
        addComponent(mOkButton, 225, 210, 125, 30);
        packComponents(400, 250);
    }

    private void initInputPanel() {

        // Input label
        mInputLabel = new JLabel("Conditional Expression:");
        mInputLabel.setBounds(10, 5, 300, 30);

        // Input text field
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 30, 300, 30);

        // Input panel
//      mInputPanel = new JPanel(null);
//      mInputPanel.setOpaque(false);
//      mInputPanel.add(mInputLabel);
//      mInputPanel.add(mInputTextField);
    }

    private void initButtonPanel() {

        // Ok button
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });

        // Cancel button
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });

        // Button panel
//      mButtonPanel = new JPanel(null);
//      mButtonPanel.setOpaque(false);
//      mButtonPanel.add(mOkButton);
//      mButtonPanel.add(mCancelButton);
    }

    protected void initAltStartNodePanel() {

        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
        mAltStartNodeLabel.setBounds(10, 5, 130, 30);

        // Init alternative start node list
        mAltStartNodeList       = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        mAltStartNodeScrollPane.setBounds(140, 10, 170, 80);

        // Init alternative start node buttons
        // add button
        mAddAltStartNodeButton = new AddButton();
        mAddAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addAltStartNode();
            }
        });

        // remove button
        mRemoveAltStartNodeButton = new RemoveButton();
        mRemoveAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                removeAltStartNode();
            }
        });

        // edit button
        mEditAltStartNodeButton = new EditButton();
        mEditAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                editAltStartNode();
            }
        });

        // Init alternative start node panel
//      mAltStartNodePanel = new JPanel(null);
//      mAltStartNodePanel.setOpaque(false);
//      mAltStartNodePanel.add(mAltStartNodeLabel);
//      mAltStartNodePanel.add(mAltStartNodeScrollPane);
//      mAltStartNodePanel.add(mAddAltStartNodeButton);
//      mAltStartNodePanel.add(mRemoveAltStartNodeButton);
//      mAltStartNodePanel.add(mEditAltStartNodeButton);
    }

    public IEdge run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mIEdge;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (process()) {
            dispose(Button.OK);
        }
        else{
            mInputTextField.setForeground(Color.red);
            Editor.getInstance().getSelectedProjectEditor().getSceneFlowEditor().setMessageLabelText("Remember to wrap condition in parenthesis");  
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    private boolean process() {
        String inputString = mInputTextField.getText().trim();

        try {
            _SFSLParser_.parseResultType = _SFSLParser_.CND;
            _SFSLParser_.run(inputString);

            Condition cnd = _SFSLParser_.cndResult;

            if ((cnd != null) &&!_SFSLParser_.errorFlag) {
                mIEdge.setCondition(cnd);

                // /
                mAltStartNodeManager.saveAltStartNodeMap();

                ////

                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            return false;
        }
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mIEdge.getTargetNode() instanceof SuperNode) {
            Iterator it = mAltStartNodeManager.mAltStartNodeMap.entrySet().iterator();

            while (it.hasNext()) {
                Map.Entry              pairs            = (Map.Entry) it.next();
                TPLTuple<String, Node> startNodePair    = (TPLTuple<String, Node>) pairs.getKey();
                TPLTuple<String, Node> altStartNodePair = (TPLTuple<String, Node>) pairs.getValue();

                ((DefaultListModel) mAltStartNodeList.getModel()).addElement(startNodePair.getFirst() + "/"
                        + altStartNodePair.getFirst());

                ////System.err.println("loading start node "+startNodePair.getSecond());
                ////System.err.println("loading alt start node "+altStartNodePair.getSecond());
            }
        } else {
            mAddAltStartNodeButton.setEnabled(false);
            mRemoveAltStartNodeButton.setEnabled(false);
            mEditAltStartNodeButton.setEnabled(false);
            mAltStartNodeList.setEnabled(false);
            mAltStartNodeScrollPane.setEnabled(false);
        }
    }

    private void saveAltStartNodeMap() {
        mAltStartNodeManager.saveAltStartNodeMap();
    }

    private void addAltStartNode() {
        CreateAltStartNodeDialog dialog = new CreateAltStartNodeDialog(mAltStartNodeManager);

        dialog.run();

        // /
        ((DefaultListModel) mAltStartNodeList.getModel()).clear();

        Iterator it = mAltStartNodeManager.mAltStartNodeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry              pairs            = (Map.Entry) it.next();
            TPLTuple<String, Node> startNodePair    = (TPLTuple<String, Node>) pairs.getKey();
            TPLTuple<String, Node> altStartNodePair = (TPLTuple<String, Node>) pairs.getValue();

            ((DefaultListModel) mAltStartNodeList.getModel()).addElement(startNodePair.getFirst() + "/"
                    + altStartNodePair.getFirst());
        }
    }

    private void removeAltStartNode() {
        String selectedValue = (String) mAltStartNodeList.getSelectedValue();

        if (selectedValue != null) {
            String[] idPair      = selectedValue.split("/");
            String   startNodeId = idPair[0];

            // String altStartNodeId = idPair[1];
            System.err.println("remove alt start node" + startNodeId);
            mAltStartNodeManager.removeAltStartNode(startNodeId);
            ((DefaultListModel) mAltStartNodeList.getModel()).removeElement(selectedValue);
        }
    }

    private void editAltStartNode() {}
}
