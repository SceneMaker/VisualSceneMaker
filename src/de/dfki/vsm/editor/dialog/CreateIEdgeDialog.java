package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.condition.Condition;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.Map;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
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

    //  The edge that should be created
    private final IEdge mIEdge;
    private final AltStartNodeManager mAltStartNodeManager;
    // GUI-Components
    private JPanel mInputPanel;
    private JLabel mInputLabel;
    private JPanel mButtonPanel;
    private JTextField mInputTextField;
    private JButton mOkButton;
    private JButton mCancelButton;
    private JPanel mAltStartNodePanel;
    private JLabel mAltStartNodeLabel;
    private JList mAltStartNodeList;
    private JScrollPane mAltStartNodeScrollPane;
    private JButton mAddAltStartNodeButton;
    private JButton mRemoveAltStartNodeButton;
    private JButton mEditAltStartNodeButton;

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
        // Init main panel
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        //
        addCompoment(mInputPanel, 320, 60);
        addCompoment(mAltStartNodePanel, 320, 100);
        addCompoment(mButtonPanel, 320, 40);
        packComponents(320, 200);
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
        mOkButton = new JButton("Ok");
        mOkButton.setBounds(110, 10, 100, 20);
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        // Cancel button
        mCancelButton = new JButton("Cancel");
        mCancelButton.setBounds(210, 10, 100, 20);
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        // Button panel
        mButtonPanel = new JPanel(null);
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(mCancelButton);
    }

    protected void initAltStartNodePanel() {
        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
        mAltStartNodeLabel.setBounds(10, 5, 130, 25);
        // Init alternative start node list
        mAltStartNodeList = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        mAltStartNodeScrollPane.setBounds(140, 10, 170, 80);
        // Init alternative start node buttons
        mAddAltStartNodeButton = new JButton("Add");
        mAddAltStartNodeButton.setBounds(15, 25, 100, 20);
        mAddAltStartNodeButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                addAltStartNode();
            }
        });
        mRemoveAltStartNodeButton = new JButton("Remove");
        mRemoveAltStartNodeButton.setBounds(15, 45, 100, 20);
        mRemoveAltStartNodeButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                removeAltStartNode();
            }
        });
        mEditAltStartNodeButton = new JButton("Edit");
        mEditAltStartNodeButton.setBounds(15, 65, 100, 20);
        mEditAltStartNodeButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                editAltStartNode();
            }
        });
        // Init alternative start node panel
        mAltStartNodePanel = new JPanel(null);
        mAltStartNodePanel.add(mAltStartNodeLabel);
        mAltStartNodePanel.add(mAltStartNodeScrollPane);
        mAltStartNodePanel.add(mAddAltStartNodeButton);
        mAltStartNodePanel.add(mRemoveAltStartNodeButton);
        mAltStartNodePanel.add(mEditAltStartNodeButton);
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
            if (cnd != null && !_SFSLParser_.errorFlag) {
                mIEdge.setCondition(cnd);

                ///
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
                Map.Entry pairs = (Map.Entry) it.next();
                TPLTuple<String, Node> startNodePair = (TPLTuple<String, Node>) pairs.getKey();
                TPLTuple<String, Node> altStartNodePair = (TPLTuple<String, Node>) pairs.getValue();
                ((DefaultListModel) mAltStartNodeList.getModel()).addElement(
                        startNodePair.getFirst() + "/" + altStartNodePair.getFirst());
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
        ///
        ((DefaultListModel) mAltStartNodeList.getModel()).clear();
        Iterator it = mAltStartNodeManager.mAltStartNodeMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry pairs = (Map.Entry) it.next();
            TPLTuple<String, Node> startNodePair = (TPLTuple<String, Node>) pairs.getKey();
            TPLTuple<String, Node> altStartNodePair = (TPLTuple<String, Node>) pairs.getValue();
            ((DefaultListModel) mAltStartNodeList.getModel()).addElement(
                    startNodePair.getFirst() + "/" + altStartNodePair.getFirst());
        }
    }

    private void removeAltStartNode() {
        String selectedValue = (String) mAltStartNodeList.getSelectedValue();
        if (selectedValue != null) {
            String[] idPair = selectedValue.split("/");
            String startNodeId = idPair[0];
            //String altStartNodeId = idPair[1];
            System.err.println("remove alt start node" + startNodeId);
            mAltStartNodeManager.removeAltStartNode(startNodeId);
            ((DefaultListModel) mAltStartNodeList.getModel()).removeElement(selectedValue);
        }
    }

    private void editAltStartNode() {
    }
}
