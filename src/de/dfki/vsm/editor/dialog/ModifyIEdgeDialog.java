package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.dialog.Dialog.Button;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.LogicalCond;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.Color;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Iterator;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import static java.awt.Component.CENTER_ALIGNMENT;
import static java.awt.Component.LEFT_ALIGNMENT;

/**
 *
 * @author Gregor Mehlmann
 */
public class ModifyIEdgeDialog extends Dialog {

    // The edge that we want to modify
    private final IEdge mIEdge;

    // GUI-Components
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

    public ModifyIEdgeDialog(IEdge iedge) {
        super(Editor.getInstance(), "Modify Interruptive Edge", true);
        mIEdge = iedge;

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mIEdge);

        // Init GUI-Components
        initComponents();
        mInputTextField.setText(mIEdge.getCondition().getConcreteSyntax());
        loadAltStartNodeMap();
    }

//  private void initComponents() {
//    mInputTextField = new JTextField();
//    mInputTextField.setBounds(10, 10, 300, 20);
//    mOkButton = new JButton("Ok");
//    mOkButton.setBounds(10, 35, 90, 20);
//    mOkButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        okActionPerformed();
//      }
//    });
//    mCancelButton = new JButton("Cancel");
//    mCancelButton.setBounds(100, 35, 90, 20);
//    mCancelButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        cancelActionPerformed();
//      }
//    });
//    addCompoment(mInputTextField, 10, 10, 300, 20);
//    addCompoment(mOkButton, 10, 35, 90, 20);
//    addCompoment(mCancelButton, 100, 35, 90, 20);
//    packComponents(320, 60);
//  }
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
        // Init main panel
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        mMainPanel.add(Box.createRigidArea(new Dimension(5, 10)));
        addCompoment(mInputPanel, 230, 40);
        mMainPanel.add(Box.createRigidArea(new Dimension(5, 10)));
        addCompoment(mAltStartNodePanel, 230, 85);
        mMainPanel.add(Box.createRigidArea(new Dimension(5, 10)));
        addCompoment(mButtonPanel, 230, 20);
        packComponents(230, 180);
    }

    private void initInputPanel() {
        JPanel panelContainer;

        // Input panel
        mInputPanel = new JPanel();
        mInputPanel.setOpaque(false);

        // Input label
        mInputLabel = new JLabel("Conditional Expression:");

        // Input text field
        mInputTextField = new JTextField();
        panelContainer  = new JPanel(null);
        panelContainer.setOpaque(false);
        panelContainer.setLayout(new BoxLayout(panelContainer, BoxLayout.Y_AXIS));
        panelContainer.add(mInputLabel);
        panelContainer.add(mInputTextField);
        mInputPanel.setLayout(new BoxLayout(mInputPanel, BoxLayout.X_AXIS));
        mInputPanel.add(Box.createRigidArea(new Dimension(3, 3)));
        mInputPanel.add(panelContainer);
        mInputPanel.add(Box.createRigidArea(new Dimension(3, 3)));
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
        mButtonPanel.setOpaque(false);
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(mCancelButton);
    }

    protected void initAltStartNodePanel() {
        JPanel titleContainer;
        JPanel buttonsContainer;
        JPanel startNodeContainer;

        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");

        // Init alternative start node list
        mAltStartNodeList       = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);

        // Init alternative start node buttons300
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
        titleContainer = new JPanel(null);
        titleContainer.setOpaque(false);
        titleContainer.setLayout(new BoxLayout(titleContainer, BoxLayout.X_AXIS));
        titleContainer.setAlignmentX(LEFT_ALIGNMENT);
        titleContainer.add(mAltStartNodeLabel);
        titleContainer.add(Box.createRigidArea(new Dimension(1000, 20)));
        buttonsContainer = new JPanel(null);
        buttonsContainer.setOpaque(false);
        buttonsContainer.setLayout(new BoxLayout(buttonsContainer, BoxLayout.Y_AXIS));
        buttonsContainer.setMaximumSize(new Dimension(20, 60));
        buttonsContainer.add(mAddAltStartNodeButton);
        buttonsContainer.add(mRemoveAltStartNodeButton);
        buttonsContainer.add(mEditAltStartNodeButton);
        startNodeContainer = new JPanel(null);
        startNodeContainer.setOpaque(false);
        startNodeContainer.setLayout(new BoxLayout(startNodeContainer, BoxLayout.X_AXIS));
        startNodeContainer.add(Box.createRigidArea(new Dimension(3, 20)));
        startNodeContainer.add(mAltStartNodeScrollPane);
        startNodeContainer.add(buttonsContainer);
        startNodeContainer.add(Box.createRigidArea(new Dimension(3, 20)));

        // Init alternative start node panel
        mAltStartNodePanel = new JPanel(null);
        mAltStartNodePanel.setOpaque(false);
        mAltStartNodePanel.setLayout(new BoxLayout(mAltStartNodePanel, BoxLayout.PAGE_AXIS));
        mAltStartNodePanel.setAlignmentX(CENTER_ALIGNMENT);
        mAltStartNodePanel.add(titleContainer);

        // cmAltStartNodePanel.add(Box.createRigidArea(new Dimension(5, 5)));
        mAltStartNodePanel.add(startNodeContainer);
    }

//  private void initComponents() {
//      mInputTextField = new JTextField();
//      mInputTextField.setBounds(10, 10, 300, 20);
//      mOkButton = new JButton("Ok");
//      mOkButton.setBounds(10, 35, 90, 20);
//      mOkButton.addActionListener(new ActionListener() {
//
//          public void actionPerformed(ActionEvent e) {
//              okActionPerformed();
//          }
//      });
//      mCancelButton = new JButton("Cancel");
//      mCancelButton.setBounds(100, 35, 90, 20);
//      mCancelButton.addActionListener(new ActionListener() {
//
//          public void actionPerformed(ActionEvent e) {
//              cancelActionPerformed();
//          }
//      });
//      addCompoment(mInputTextField, 10, 10, 300, 20);
//      addCompoment(mOkButton, 10, 35, 90, 20);
//      addCompoment(mCancelButton, 100, 35, 90, 20);
//      packComponents(320, 60);
//  }
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
            _SFSLParser_.parseResultType = _SFSLParser_.LOG;
            _SFSLParser_.run(inputString);

            LogicalCond log = _SFSLParser_.logResult;

            if ((log != null) &&!_SFSLParser_.errorFlag) {
                mIEdge.setCondition(log);

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

    public JPanel getInputPanel() {
        return mInputPanel;
    }

    public JPanel getAltStartNodePanel() {
        return mAltStartNodePanel;
    }

    public JPanel getButtonPanel() {
        return mButtonPanel;
    }

    public JTextField getInputTextField() {
        return mInputTextField;
    }
}
