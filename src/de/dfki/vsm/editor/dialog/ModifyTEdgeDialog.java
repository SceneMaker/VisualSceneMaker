package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

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

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;

import java.util.Iterator;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

/**
 * @author Patrick Gebhard
 *
 */
public class ModifyTEdgeDialog extends Dialog {

    // The edge that should be created
    private final TEdge mTEdge;

    // private HashMap<Pair<String, Node>, Pair<String, Node>> mAltStartNodeMap = null;
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

    public ModifyTEdgeDialog(TEdge tedge) {
        super(Editor.getInstance(), "Modify Timeout Edge:", true);
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

        // Init alternative start node panel
        initAltStartNodePanel();

        // Init button panel
        initButtonPanel();

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

//  private void initComponents() {
//    mInputTextField = new JTextField();
//    mInputTextField.setBounds(10, 10, 300, 20);
//    mOkButton = new JButton("Ok");
//    mOkButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        okActionPerformed();
//      }
//    });
//    mCancelButton = new JButton("Cancel");
//    mCancelButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        cancelActionPerformed();
//      }
//    });
//    mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
//    mAltStartNodeList = new JList(new DefaultListModel());
//    mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
//    mAddAltStartNodeButton = new JButton("Add");
//    mAddAltStartNodeButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        addAltStartNode();
//      }
//    });
//    mRemoveAltStartNodeButton = new JButton("Remove");
//    mRemoveAltStartNodeButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        removeAltStartNode();
//      }
//    });
//    mEditAltStartNodeButton = new JButton("Edit");
//    mEditAltStartNodeButton.addActionListener(new ActionListener() {
//
//      public void actionPerformed(ActionEvent e) {
//        editAltStartNode();
//      }
//    });
//    addCompoment(mInputTextField, 10, 10, 300, 20);
//    addCompoment(mAltStartNodeLabel, 10, 35, 130, 25);
//    addCompoment(mAltStartNodeScrollPane, 140, 35, 170, 80);
//    addCompoment(mAddAltStartNodeButton, 10, 60, 100, 20);
//    addCompoment(mRemoveAltStartNodeButton, 10, 80, 100, 20);
//    addCompoment(mEditAltStartNodeButton, 10, 100, 100, 20);
//    addCompoment(mOkButton, 130, 125, 90, 20);
//    addCompoment(mCancelButton, 220, 125, 90, 20);
//    packComponents(320, 160);
//  }
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

    public JTextField getInputTextField() {
        return mInputTextField;
    }

    private void initInputPanel() {
        JPanel panelContainer;

        // Input panel
        mInputPanel = new JPanel();
        mInputPanel.setOpaque(false);

        // Input label
        mInputLabel = new JLabel("Timeout Value:");

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
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.setAlignmentX(CENTER_ALIGNMENT);
        mButtonPanel.add(Box.createRigidArea(new Dimension(45, 20)));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createRigidArea(new Dimension(15, 20)));
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createRigidArea(new Dimension(15, 20)));
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

        // mAltStartNodePanel.add(Box.createRigidArea(new Dimension(5, 5)));
        mAltStartNodePanel.add(startNodeContainer);
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
