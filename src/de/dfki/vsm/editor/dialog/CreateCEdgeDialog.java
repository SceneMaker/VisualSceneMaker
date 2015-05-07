package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.LogicalCond;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.tpl.TPLTuple;

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
 * @author Gregor Mehlmann
 */
public class CreateCEdgeDialog extends Dialog {

    // The edge that should be created
    private final CEdge               mCEdge;
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

    public CreateCEdgeDialog(Node sourceNode, Node targetNode) {
        super(Editor.getInstance(), "Create Conditional Edge", true);

        // Set the edge data
        mCEdge = new CEdge();
        mCEdge.setTarget(targetNode.getId());
        mCEdge.setSourceNode(sourceNode);
        mCEdge.setTargetNode(targetNode);

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mCEdge);

        // Init GUI-Components
        initComponents();
    }

    private void initComponents() {

        // Init input panel
        initInputPanel();

        // Init button panel
        initButtonPanel();

        // Init alternative start node panel
        initAltStartNodePanel();

        // Init main panel
//      mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        //
//      addCompoment(mInputPanel, 320, 60);
//      addCompoment(mAltStartNodePanel, 320, 100);
//      addCompoment(mButtonPanel, 320, 40);
        //
        addCompoment(mInputLabel, 10, 10, 70, 30);
        addCompoment(mInputTextField, 120, 10, 230, 30);

        //
        addCompoment(mAltStartNodeLabel, 10, 75, 70, 30);
        addCompoment(mAltStartNodeScrollPane, 120, 75, 230, 110);
        addCompoment(mAddAltStartNodeButton, 355, 85, 20, 20);
        addCompoment(mRemoveAltStartNodeButton, 355, 115, 20, 20);
        addCompoment(mEditAltStartNodeButton, 355, 145, 20, 20);

        //
        addCompoment(mCancelButton, 75, 210, 125, 30);
        addCompoment(mOkButton, 225, 210, 125, 30);
        packComponents(400, 250);
    }

    private void initInputPanel() {

        // Input label
        mInputLabel = new JLabel("Conditional Expression:");
        mInputLabel.setBounds(10, 5, 300, 25);

        // Input text field
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 30, 300, 25);

        // Input panel
        mInputPanel = new JPanel(null);
        mInputPanel.add(mInputLabel);
        mInputPanel.add(mInputTextField);
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
//      mButtonPanel.add(mOkButton);
//      mButtonPanel.add(mCancelButton);
    }

    protected void initAltStartNodePanel() {

        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
        mAltStartNodeLabel.setBounds(10, 5, 130, 25);

        // Init alternative start node list
        mAltStartNodeList       = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        mAltStartNodeScrollPane.setBounds(140, 10, 170, 80);

        // Init alternative start node buttons
        // ADD BUTTON
        mAddAltStartNodeButton = new AddButton();
        mAddAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addAltStartNode();
            }
        });

        // REMOVE BUTTON
        mRemoveAltStartNodeButton = new RemoveButton();
        mRemoveAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                removeAltStartNode();
            }
        });

        // EDIT BUTTON
        mEditAltStartNodeButton = new EditButton();
        mEditAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                editAltStartNode();
            }
        });

        // Init alternative start node panel
//      mAltStartNodePanel = new JPanel(null);
//      mAltStartNodePanel.add(mAltStartNodeLabel);
//      mAltStartNodePanel.add(mAltStartNodeScrollPane);
//      mAltStartNodePanel.add(mAddAltStartNodeButton);
//      mAltStartNodePanel.add(mRemoveAltStartNodeButton);
//      mAltStartNodePanel.add(mEditAltStartNodeButton);
    }

    public CEdge run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mCEdge;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (process()) {
            dispose(Button.OK);
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    private boolean process() {
        String inputString = mInputTextField.getText().trim();

        try {
            _SFSLParser_.parseResultType = _SFSLParser_.LOG;
            _SFSLParser_.run(inputString);

            LogicalCond log = _SFSLParser_.logResult;

            if ((log != null) &&!_SFSLParser_.errorFlag) {
                mCEdge.setCondition(log);

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

        if (mCEdge.getTargetNode() instanceof SuperNode) {
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
