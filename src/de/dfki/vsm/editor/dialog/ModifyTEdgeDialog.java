package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.Color;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;

import java.util.Iterator;
import java.util.Map;
import javax.swing.BorderFactory;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 *
 */
public class ModifyTEdgeDialog extends Dialog {

    // The edge that should be created
    private final TEdge mTEdge;

    // private HashMap<Pair<String, BasicNode>, Pair<String, BasicNode>> mAltStartNodeMap = null;
    private final AltStartNodeManager mAltStartNodeManager;

    // GUI-Components
    private JPanel       mInputPanel;
    private JLabel       mInputLabel;
    private JPanel       mButtonPanel;
    private HintTextField   mInputTextField;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private JPanel       mAltStartNodePanel;
    private JLabel       mAltStartNodeLabel;
    private JList        mAltStartNodeList;
    private JScrollPane  mAltStartNodeScrollPane;
    private AddButton    mAddAltStartNodeButton;
    private RemoveButton mRemoveAltStartNodeButton;
    private EditButton   mEditAltStartNodeButton;
    private Dimension labelSize = new Dimension(200, 30);
    private Dimension textFielSize = new Dimension(230, 30);
    private JLabel errorMsg;

    public ModifyTEdgeDialog(BasicNode sourceNode, BasicNode targetNode) {
        super(EditorInstance.getInstance(), "Create Timeout Edge", true);

        // Set the edge data
        mTEdge = new TEdge();
        mTEdge.setTarget(targetNode.getId());
        mTEdge.setSourceNode(sourceNode);
        mTEdge.setTargetNode(targetNode);

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mTEdge);

        // Init GUI-Components
        initComponents();
    }
    
    public ModifyTEdgeDialog(TEdge tedge) {
        super(EditorInstance.getInstance(), "Modify Timeout Edge:", true);
        mTEdge = tedge;

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mTEdge);

        // Init GUI-Components
        initComponents();

        String timeout = Long.toString(mTEdge.getTimeout());

        timeout = (timeout == null)
                  ? "1000"
                  : (timeout.isEmpty())
                    ? "1000"
                    : timeout;
        mInputTextField.setText(Long.toString(mTEdge.getTimeout()));
        loadAltStartNodeMap();
    }

    private void initComponents() {

        // Init input panel
        initInputPanel();
        
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(labelSize);
        
        // Init alternative start node panel
        initAltStartNodePanel();

        // Init button panel
        initButtonPanel();

        // Init main panel
        Box finalBox = Box.createVerticalBox();
        finalBox.setAlignmentX(CENTER_ALIGNMENT);
        finalBox.add(mInputPanel);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mAltStartNodePanel);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mButtonPanel);

        addComponent(finalBox, 10, 30, 480, 280);

        packComponents(520, 300);
        mOkButton.requestFocus();
    }

    public JPanel getInputPanel() {
        return mInputPanel;
    }

    public JPanel getButtonPanel() {
        return mButtonPanel;
    }

    public OKButton getOKButton() {
        return mOkButton;
    }

    public JPanel getAltStartNodePanel() {
        return mAltStartNodePanel;
    }

    public HintTextField getInputTextField() {
        return mInputTextField;
    }

    private void initInputPanel() {
        // Input label
        mInputLabel = new JLabel("Timeout Value: ");
        sanitizeComponent(mInputLabel, labelSize);
        // Input text field
        mInputTextField = new HintTextField("1000");
        mInputTextField.setText("1000");
        sanitizeComponent(mInputTextField, textFielSize);
        // Input panel
        mInputPanel = new JPanel();
        mInputPanel.setLayout(new BoxLayout(mInputPanel, BoxLayout.X_AXIS));
        mInputPanel.add(mInputLabel);
        mInputPanel.add(Box.createHorizontalStrut(10));
        mInputPanel.add(mInputTextField);
    }
    /**
     * Set the correct size of the components
     * @param jb
     * @param dim 
     */
    private void sanitizeComponent(JComponent jb, Dimension dim) {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
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
        mButtonPanel = new JPanel();
        mButtonPanel.setMinimumSize(new Dimension(440, 40));
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
    }

    protected void initAltStartNodePanel() {
        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
        sanitizeComponent(mAltStartNodeLabel, labelSize);
        // Init alternative start node list
        mAltStartNodeList       = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        mAltStartNodeScrollPane.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        Dimension tfSize = new Dimension(200, 110);
        mAltStartNodeScrollPane.setPreferredSize(tfSize);
        mAltStartNodeScrollPane.setMinimumSize(tfSize);
        mAltStartNodeScrollPane.setMaximumSize(tfSize);
        // Init alternative start node buttons
        //add button
        mAddAltStartNodeButton = new AddButton();
        mAddAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addAltStartNode();
            }
        });
        //remove button
        mRemoveAltStartNodeButton = new RemoveButton();
        mRemoveAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                removeAltStartNode();
            }
        });
        //edit button
        mEditAltStartNodeButton = new EditButton();
        mEditAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                editAltStartNode();
            }
        });
        // Init alternative start node panel
        Box buttonsBox = Box.createVerticalBox();
        buttonsBox.setMaximumSize(new Dimension(20, 100));
        buttonsBox.add(mAddAltStartNodeButton);
        buttonsBox.add(Box.createVerticalStrut(10));
        buttonsBox.add(mRemoveAltStartNodeButton);
        buttonsBox.add(Box.createVerticalStrut(10));
        buttonsBox.add(mEditAltStartNodeButton);
        mAltStartNodePanel = new JPanel();
        mAltStartNodePanel.setLayout(new BoxLayout(mAltStartNodePanel, BoxLayout.X_AXIS));
        mAltStartNodePanel.add(mAltStartNodeLabel);
        mAltStartNodePanel.add(Box.createHorizontalStrut(10));
        mAltStartNodePanel.add(mAltStartNodeScrollPane);
        mAltStartNodePanel.add(Box.createHorizontalStrut(10));
        mAltStartNodePanel.add(buttonsBox);
    }

    public TEdge run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mTEdge;
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
        if(mInputTextField.getText().length() == 0){
            mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);

            return false;
        }
        String inputString = mInputTextField.getText().trim();

        try {
            long timeout = Long.valueOf(inputString);

            mTEdge.setTimeout(timeout);

            // /
            mAltStartNodeManager.saveAltStartNodeMap();

            ////
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mTEdge.getTargetNode() instanceof SuperNode) {
            Iterator it = mAltStartNodeManager.mAltStartNodeMap.entrySet().iterator();

            while (it.hasNext()) {
                Map.Entry              pairs            = (Map.Entry) it.next();
                TPLTuple<String, BasicNode> startNodePair    = (TPLTuple<String, BasicNode>) pairs.getKey();
                TPLTuple<String, BasicNode> altStartNodePair = (TPLTuple<String, BasicNode>) pairs.getValue();

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
            TPLTuple<String, BasicNode> startNodePair    = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodePair = (TPLTuple<String, BasicNode>) pairs.getValue();

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
