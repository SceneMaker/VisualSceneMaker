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
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

public class CreatePEdgeDialog extends Dialog {

    //
    private HashMap<PEdge, JTextField> mPEdgeMap  = new HashMap<PEdge, JTextField>();
    private final Dimension            buttonSize = new Dimension(125, 30);

    // Data model components
    private final Node  mSourceNode;
    private final Node  mTargetNode;
    private final PEdge mPEdge;

    //
    private final AltStartNodeManager mAltStartNodeManager;

    // GUI-Components
    private JPanel mHeaderPanel;
    private JLabel mHeaderLabel;

    //
    private JPanel       mButtonPanel;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private JLabel       mNormButton;
    private JLabel       mUniButton;

    //
    private JPanel       mAltStartNodePanel;
    private JLabel       mAltStartNodeLabel;
    private JList        mAltStartNodeList;
    private JScrollPane  mAltStartNodeScrollPane;
    private AddButton    mAddAltStartNodeButton;
    private RemoveButton mRemoveAltStartNodeButton;
    private EditButton   mEditAltStartNodeButton;

    //
    private JPanel mEdgeProbPanel;

    //
    private JPanel     mRestPanel;
    private JLabel     mRestLabel;
    private JTextField mRestField;

    public CreatePEdgeDialog(Node sourceNode, Node targetNode) {
        super(Editor.getInstance(), "Create Probability Edge", true);
        mSourceNode = sourceNode;
        mTargetNode = targetNode;

        // Create a new probability edge
        mPEdge = new PEdge();
        mPEdge.setTarget(mTargetNode.getId());
        mPEdge.setSource(mSourceNode.getId());
        mPEdge.setSourceNode(mSourceNode);
        mPEdge.setTargetNode(mTargetNode);

        // Fill the local data map with the existing edges
        for (PEdge edge : mSourceNode.getPEdgeList()) {
            mPEdgeMap.put(edge, null);
        }

        // The current edge
        mPEdgeMap.put(mPEdge, null);

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mPEdge);

        // Init the GUI-Components
        initComponents();

        //
        loadAltStartNodeMap();
    }

    private void initComponents() {

        // Init probability panel
        initEdgeProbabilityPanel();

        // Init button panel
        initButtonPanel();

        // Init alternative start node panel
        initAltStartNodePanel();

        //
        addCompoment(mEdgeProbPanel, 0, 0, 400, 110);

        //
        addCompoment(mAltStartNodeLabel, 10, 125, 70, 30);
        addCompoment(mAltStartNodeScrollPane, 90, 125, 260, 110);
        addCompoment(mAddAltStartNodeButton, 355, 125, 20, 20);
        addCompoment(mRemoveAltStartNodeButton, 355, 155, 20, 20);
        addCompoment(mEditAltStartNodeButton, 355, 185, 20, 20);

        //
        addCompoment(mNormButton, 75, 285, 125, 30);
        addCompoment(mUniButton, 225, 285, 125, 30);
        addCompoment(mCancelButton, 75, 335, 125, 30);
        addCompoment(mOkButton, 225, 335, 125, 30);

//      addCompoment(mAltStartNodePanel, 320, 100);
//      addCompoment(mButtonPanel, 320, 40);
        packComponents(400, 400);
    }

    private void initButtonPanel() {

        // Normalize button
        mNormButton = new JLabel("Normalize");
        mNormButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mNormButton.setOpaque(true);
        mNormButton.setBackground(Color.white);
        mNormButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mNormButton.setToolTipText("Normalize");
        mNormButton.setIcon(ResourceLoader.loadImageIcon("/res/img/normalize_gray.png"));
        mNormButton.setIconTextGap(20);
        mNormButton.setFont(new Font("Helvetica", Font.PLAIN, 20));
        mNormButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mNormButton.setPreferredSize(buttonSize);
        mNormButton.setMinimumSize(buttonSize);
        mNormButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                normalizeActionPerformed();
            }
            public void mouseEntered(MouseEvent me) {
                mNormButton.setIcon(ResourceLoader.loadImageIcon("/res/img/normalize_blue.png"));
                mNormButton.setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                mNormButton.setIcon(ResourceLoader.loadImageIcon("/res/img/normalize_gray.png"));
                mNormButton.setBackground(new Color(255, 255, 255));
            }
        });

        // Uniform button
        mUniButton = new JLabel("Uniform");
        mUniButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mUniButton.setOpaque(true);
        mUniButton.setBackground(Color.white);
        mUniButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mUniButton.setToolTipText("Uniform");
        mUniButton.setIcon(ResourceLoader.loadImageIcon("/res/img/uniform_gray.png"));
        mUniButton.setIconTextGap(20);
        mUniButton.setFont(new Font("Helvetica", Font.PLAIN, 20));
        mUniButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mUniButton.setPreferredSize(buttonSize);
        mUniButton.setMinimumSize(buttonSize);
        mUniButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                uniformActionPerformed();
            }
            public void mouseEntered(MouseEvent me) {
                mUniButton.setIcon(ResourceLoader.loadImageIcon("/res/img/uniform_blue.png"));
                mUniButton.setBackground(new Color(82, 127, 255));
            }
            public void mouseExited(MouseEvent me) {
                mUniButton.setIcon(ResourceLoader.loadImageIcon("/res/img/uniform_gray.png"));
                mUniButton.setBackground(new Color(255, 255, 255));
            }
        });

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
//      mButtonPanel.add(mNormButton);
//      mButtonPanel.add(mUniButton);
//      mButtonPanel.add(mOkButton);
//      mButtonPanel.add(mCancelButton);
    }

    private void initEdgeProbabilityPanel() {

        // Init header label
        mHeaderLabel = new JLabel("Edge Probabilities:");
        mHeaderLabel.setMinimumSize(new Dimension(240, 30));
        mHeaderLabel.setPreferredSize(new Dimension(240, 30));
        mHeaderLabel.setMaximumSize(new Dimension(240, 30));

        // Init header panel
        mHeaderPanel = new JPanel();
        mHeaderPanel.setOpaque(false);
        mHeaderPanel.setMinimumSize(new Dimension(400, 30));
        mHeaderPanel.setPreferredSize(new Dimension(400, 30));
        mHeaderPanel.setMaximumSize(new Dimension(400, 30));
        mHeaderPanel.setLayout(new BoxLayout(mHeaderPanel, BoxLayout.X_AXIS));
        mHeaderPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mHeaderPanel.add(mHeaderLabel);

        // Init edge probability panel
        mEdgeProbPanel = new JPanel();
        mEdgeProbPanel.setOpaque(false);
        mEdgeProbPanel.setLayout(new BoxLayout(mEdgeProbPanel, BoxLayout.Y_AXIS));
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(400, 5)));
        mEdgeProbPanel.add(mHeaderPanel);
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(400, 5)));

        //
        for (PEdge pedge : mPEdgeMap.keySet()) {

            // Init description label
            JLabel pedgeDescription = new JLabel(mSourceNode.getName() + " ( " + mSourceNode.getId() + " ) "
                                          + " \u2192  " + pedge.getTargetNode().getName() + " ( "
                                          + pedge.getTargetNode().getId() + " ) ");

            pedgeDescription.setMinimumSize(new Dimension(240, 30));
            pedgeDescription.setPreferredSize(new Dimension(240, 30));
            pedgeDescription.setMaximumSize(new Dimension(240, 30));

            // Compute initial probability
            int prob = pedge.getProbability();

            if (prob == Integer.MIN_VALUE) {
                prob = 100;
            }

            // Init probability text field
            JTextField probField = new JTextField(new IntegerDocument(), String.valueOf(prob), 3);

            probField.setMinimumSize(new Dimension(40, 30));
            probField.setPreferredSize(new Dimension(40, 30));
            probField.setMaximumSize(new Dimension(40, 30));
            probField.addCaretListener(new CaretListener() {
                public void caretUpdate(CaretEvent e) {
                    int sum = 0;

                    for (JTextField textField : mPEdgeMap.values()) {
                        try {
                            sum += Integer.valueOf(textField.getText().trim()).intValue();
                        } catch (NumberFormatException es) {

                            //
                        }
                    }

                    // Set the rest to the rest text field
                    mRestField.setText(Integer.valueOf(100 - sum).toString());
                }
            });

            // Add the text field to the mapping
            mPEdgeMap.put(pedge, probField);

            // Init probability panel
            JPanel pedgePanel = new JPanel();

            pedgePanel.setOpaque(false);
            pedgePanel.setMinimumSize(new Dimension(400, 30));
            pedgePanel.setPreferredSize(new Dimension(400, 30));
            pedgePanel.setMaximumSize(new Dimension(400, 30));
            pedgePanel.setLayout(new BoxLayout(pedgePanel, BoxLayout.X_AXIS));
            pedgePanel.add(Box.createRigidArea(new Dimension(20, 0)));
            pedgePanel.add(pedgeDescription);
            pedgePanel.add(Box.createRigidArea(new Dimension(40, 0)));

//          pedgePanel.add(Box.createHorizontalGlue());
            pedgePanel.add(probField);
            pedgePanel.add(Box.createRigidArea(new Dimension(10, 0)));
            mEdgeProbPanel.add(pedgePanel);
        }

        // Init rest panel
        mRestPanel = new JPanel();
        mRestPanel.setOpaque(false);
        mRestPanel.setLayout(new BoxLayout(mRestPanel, BoxLayout.X_AXIS));
        mRestPanel.setMinimumSize(new Dimension(400, 30));
        mRestPanel.setPreferredSize(new Dimension(400, 30));
        mRestPanel.setMaximumSize(new Dimension(400, 30));

        //
        mRestLabel = new JLabel("Rest:");
        mRestLabel.setMinimumSize(new Dimension(240, 30));
        mRestLabel.setPreferredSize(new Dimension(240, 30));
        mRestLabel.setMaximumSize(new Dimension(240, 30));

        //
        mRestField = new JTextField(3);
        mRestField.setDocument(new IntegerDocument());
        mRestField.setEditable(false);
        mRestField.setEnabled(false);
        mRestField.setMinimumSize(new Dimension(40, 30));
        mRestField.setPreferredSize(new Dimension(40, 30));
        mRestField.setMaximumSize(new Dimension(40, 30));
        mRestPanel.add(Box.createRigidArea(new Dimension(20, 0)));
        mRestPanel.add(mRestLabel);
        mRestPanel.add(Box.createRigidArea(new Dimension(40, 0)));

//      mRestPanel.add(Box.createHorizontalGlue());
        mRestPanel.add(mRestField);
        mRestPanel.add(Box.createRigidArea(new Dimension(10, 0)));

        // Add the rest panel
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(320, 5)));
        mEdgeProbPanel.add(mRestPanel);
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(320, 5)));

        // Highlight the prbability text field of the current edge
        mPEdgeMap.get(mPEdge).setCaretPosition(0);
        mPEdgeMap.get(mPEdge).setForeground(Color.red);
        mPEdgeMap.get(mPEdge).setForeground(Color.red);
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
//      mAltStartNodePanel.add(mAltStartNodeLabel);
//      mAltStartNodePanel.add(mAltStartNodeScrollPane);
//      mAltStartNodePanel.add(mAddAltStartNodeButton);
//      mAltStartNodePanel.add(mRemoveAltStartNodeButton);
//      mAltStartNodePanel.add(mEditAltStartNodeButton);
    }

    private void saveProbabilities() {
        if (!areProbabilitiesValid()) {
            return;
        }

        // Save the probabilities
        Iterator it = mPEdgeMap.entrySet().iterator();

        while (it.hasNext()) {
            Map.Entry  entry = (Map.Entry) it.next();
            PEdge      edge  = (PEdge) entry.getKey();
            JTextField field = (JTextField) entry.getValue();

            edge.setProbability(Integer.valueOf(field.getText().trim()));
        }
    }

    private boolean areProbabilitiesValid() {
        int sum = 0;

        for (JTextField textField : mPEdgeMap.values()) {
            try {
                sum += Integer.valueOf(textField.getText().trim()).intValue();
            } catch (NumberFormatException e) {
                return false;
            }
        }

        return (sum == 100);
    }

    private void normalizeActionPerformed() {

        // Compute the total sum of all probabilities
        int sum = 0;

        for (JTextField textField : mPEdgeMap.values()) {
            try {
                int value = Integer.valueOf(textField.getText().trim());

                if (value <= 0) {

                    // ERROR
                } else {
                    sum += value;
                }
            } catch (NumberFormatException e) {

                // ERROR
            }
        }

        for (JTextField textField : mPEdgeMap.values()) {
            double prob    = Integer.valueOf(textField.getText().trim()).doubleValue();
            double ratiuon = (prob / Integer.valueOf(sum).doubleValue()) * 100.0d;

            textField.setText(Integer.valueOf((int) Math.round(ratiuon)).toString());
        }
    }

    private void uniformActionPerformed() {
        int numEdges = mPEdgeMap.size();
        int uniProb  = 100 / numEdges;
        int restVal  = 100 % numEdges;

        for (JTextField textField : mPEdgeMap.values()) {
            textField.setText(Integer.toString(uniProb));
        }

//      for (JTextField textField : mPEdgeMap.values()) {
//
//          if (restVal == 0) {
//              break;
//          }
//          textField.setText(Integer.toString(Integer.valueOf(textField.getText().trim()) + 1));
//          restVal--;
//      }
    }

    protected void okActionPerformed() {
        if (areProbabilitiesValid()) {
            saveProbabilities();
            saveAltStartNodeMap();
            dispose(Button.OK);
        }
    }

    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    public PEdge run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mPEdge;
        } else {
            return null;
        }
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mPEdge.getTargetNode() instanceof SuperNode) {
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


/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
class IntegerDocument extends PlainDocument {
    @Override
    public void insertString(int offset, String s, AttributeSet attributeSet) throws BadLocationException {
        try {
            Integer.parseInt(s);
        } catch (Exception e) {
            Toolkit.getDefaultToolkit().beep();

            return;
        }

        super.insertString(offset, s, attributeSet);
    }
}
