package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.util.tpl.TPLTuple;
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
 * @author Patrick Gebhard
 */
public class CreateTEdgeDialog extends Dialog {

    // The edge that should be created
    private final TEdge mTEdge;
    private final AltStartNodeManager mAltStartNodeManager;
    // GUI-Components
    private JPanel mInputPanel;
    private JLabel mInputLabel;
    private JPanel mButtonPanel;
    private JTextField mInputTextField;
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private JPanel mAltStartNodePanel;
    private JLabel mAltStartNodeLabel;
    private JList mAltStartNodeList;
    private JScrollPane mAltStartNodeScrollPane;
    private AddButton mAddAltStartNodeButton;
    private RemoveButton mRemoveAltStartNodeButton;
    private EditButton mEditAltStartNodeButton;

    public CreateTEdgeDialog(Node sourceNode, Node targetNode) {
        super(Editor.getInstance(), "Create Timeout Edge", true);
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

    private void initComponents() {
        // Init input panel
        initInputPanel();
        // Init button panel
        initButtonPanel();
        // Init alternative start node panel
        initAltStartNodePanel();
        // Init main panel
//        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        //
//        addComponent(mInputPanel, 320, 60);
//        addComponent(mAltStartNodePanel, 320, 100);
//        addComponent(mButtonPanel, 320, 40);
        addComponent(mInputLabel, 10, 10, 70, 30);
        addComponent(mInputTextField, 120, 10, 230, 30);
        //
        addComponent(mAltStartNodeLabel, 10, 75, 70, 30);
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
        mInputLabel = new JLabel("Timeout Value:");
        mInputLabel.setBounds(10, 5, 300, 25);
        // Input text field
        mInputTextField = new JTextField();
        mInputTextField.setBounds(10, 30, 300, 25);
        // sets 1000 as default
        mInputTextField.setText("1000");
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
        mAddAltStartNodeButton = new AddButton();
        mAddAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addAltStartNode();
            }
        });
        mRemoveAltStartNodeButton = new RemoveButton();
        mRemoveAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                removeAltStartNode();
            }
        });
        mEditAltStartNodeButton = new EditButton();
        mEditAltStartNodeButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
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

//  private void initComponents() {
//    mInputTextField = new JTextField();
//    Dimension tSize = new Dimension(150,30);
//    mInputTextField.setMinimumSize(tSize);
//    mInputTextField.setPreferredSize(tSize);
//    // Set initial value
//    mInputTextField.setText("1000");
//    mInputTextField.addKeyListener(new KeyListener() {
//      public void keyReleased(KeyEvent ke) {
//      }
//
//      public void keyPressed(KeyEvent ke) {
//        if (KeyEvent.VK_ENTER == ke.getKeyCode()) {
//          okActionPerformed();
//        }
//      }
//
//      public void keyTyped(KeyEvent ke) {
//      }
//    });
//
//    // create buttons
//    mOkButton = new JButton("Ok");
//    mOkButton.addActionListener(new ActionListener() {
//      public void actionPerformed(ActionEvent evt) {
//        okActionPerformed();
//      }
//    });
//
//    mOkButton.setSelected(true);
//    mCancelButton = new JButton("Cancel");
//    mCancelButton.addActionListener(new ActionListener() {
//      public void actionPerformed(ActionEvent evt) {
//        cancelActionPerformed();
//      }
//    });
//    // build panels
//    mInputPanel = new JPanel();
//    mInputPanel.setLayout(new BoxLayout(mInputPanel, BoxLayout.X_AXIS));
//    mInputPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//    mInputPanel.add(new JLabel("Milliseconds"));
//    mInputPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//    mInputPanel.add(mInputTextField);
//    mInputPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//    mButtonsPanel = new JPanel();
//    mButtonsPanel.setLayout(new BoxLayout(mButtonsPanel, BoxLayout.X_AXIS));
//    mButtonsPanel.add(Box.createHorizontalGlue());
//    mButtonsPanel.add(mCancelButton);
//    mButtonsPanel.add(mOkButton);
//    mButtonsPanel.add(Box.createRigidArea(new Dimension(5, 0)));
//    mMainPanel = new JPanel();
//    mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
//    mMainPanel.add(Box.createRigidArea(new Dimension(0, 15)));
//    mMainPanel.add(mInputPanel);
//    mMainPanel.add(Box.createRigidArea(new Dimension(0, 15)));
//    mMainPanel.add(mButtonsPanel);
//    mMainPanel.add(Box.createRigidArea(new Dimension(0, 5)));
//    add(mMainPanel);
//    setResizable(false);
//    pack();
//    setLocation(
//      getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
//      getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
//
//  }
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
        String inputString = mInputTextField.getText().trim();
        try {
            long timeout = Long.valueOf(inputString);
            mTEdge.setTimeout(timeout);

            ///
            mAltStartNodeManager.saveAltStartNodeMap();
            ////
            return true;
//      parser.parseResultType = parser.LOG;
//      parser.run(inputString);
//      LogicalCond log = parser.logResult;
//      if (log != null && !parser.errorFlag) {
//        mCEdge.setCondition(log);
//        return true;
//      } else {
//        return false;
//      }
        } catch (NumberFormatException e) {
            return false;
        }
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mTEdge.getTargetNode() instanceof SuperNode) {
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
