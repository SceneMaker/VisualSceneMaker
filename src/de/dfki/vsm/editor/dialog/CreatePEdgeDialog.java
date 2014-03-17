package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
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
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

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

public class CreatePEdgeDialog extends Dialog {

    // Data model components
    private final Node mSourceNode;
    private final Node mTargetNode;
    private final PEdge mPEdge;
    //
    private HashMap<PEdge, JTextField> mPEdgeMap
            = new HashMap<PEdge, JTextField>();
    //
    private final AltStartNodeManager mAltStartNodeManager;
    // GUI-Components
    private JPanel mHeaderPanel;
    private JLabel mHeaderLabel;
    //
    private JPanel mButtonPanel;
    private JButton mOkButton;
    private JButton mCancelButton;
    private JButton mNormButton;
    private JButton mUniButton;
    //
    private JPanel mAltStartNodePanel;
    private JLabel mAltStartNodeLabel;
    private JList mAltStartNodeList;
    private JScrollPane mAltStartNodeScrollPane;
    private JButton mAddAltStartNodeButton;
    private JButton mRemoveAltStartNodeButton;
    private JButton mEditAltStartNodeButton;
    //
    private JPanel mEdgeProbPanel;
    //
    private JPanel mRestPanel;
    private JLabel mRestLabel;
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
        // Init main panel
        // Init main panel
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS));
        //
        addCompoment(mEdgeProbPanel, 320, 70 + 25 * mPEdgeMap.size());
        addCompoment(mAltStartNodePanel, 320, 100);
        addCompoment(mButtonPanel, 320, 40);
        packComponents(320, 140 + 70 + 25 * mPEdgeMap.size());
    }

    private void initButtonPanel() {
        // Normalize button
        mNormButton = new JButton("Normalize");
        mNormButton.setBounds(10, 10, 80, 20);
        mNormButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                normalizeActionPerformed();
            }
        });
        // Uniform button
        mUniButton = new JButton("Uniform");
        mUniButton.setBounds(90, 10, 80, 20);
        mUniButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                uniformActionPerformed();
            }
        });
        // Ok button
        mOkButton = new JButton("Ok");
        mOkButton.setBounds(170, 10, 70, 20);
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        // Cancel button
        mCancelButton = new JButton("Cancel");
        mCancelButton.setBounds(240, 10, 70, 20);
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        // Button panel
        mButtonPanel = new JPanel(null);
        mButtonPanel.add(mNormButton);
        mButtonPanel.add(mUniButton);
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(mCancelButton);
    }

    private void initEdgeProbabilityPanel() {
        // Init header label
        mHeaderLabel = new JLabel("Edge Probabilities:");
        mHeaderLabel.setMinimumSize(new Dimension(240, 25));
        mHeaderLabel.setPreferredSize(new Dimension(240, 25));
        mHeaderLabel.setMaximumSize(new Dimension(240, 25));
        // Init header panel
        mHeaderPanel = new JPanel();
        mHeaderPanel.setMinimumSize(new Dimension(320, 25));
        mHeaderPanel.setPreferredSize(new Dimension(320, 25));
        mHeaderPanel.setMaximumSize(new Dimension(320, 25));
        mHeaderPanel.setLayout(new BoxLayout(mHeaderPanel, BoxLayout.X_AXIS));
        mHeaderPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mHeaderPanel.add(mHeaderLabel);
        // Init edge probability panel
        mEdgeProbPanel = new JPanel();
        mEdgeProbPanel.setLayout(new BoxLayout(mEdgeProbPanel, BoxLayout.Y_AXIS));
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(320, 5)));
        mEdgeProbPanel.add(mHeaderPanel);
        mEdgeProbPanel.add(Box.createRigidArea(new Dimension(320, 5)));

        //
        for (PEdge pedge : mPEdgeMap.keySet()) {
            // Init description label
            JLabel pedgeDescription = new JLabel(
                    mSourceNode.getName() + " ( "
                    + mSourceNode.getId() + " ) " + " \u2192  "
                    + pedge.getTargetNode().getName() + " ( "
                    + pedge.getTargetNode().getId() + " ) ");
            pedgeDescription.setMinimumSize(new Dimension(240, 25));
            pedgeDescription.setPreferredSize(new Dimension(240, 25));
            pedgeDescription.setMaximumSize(new Dimension(240, 25));
            // Compute initial probability
            int prob = pedge.getProbability();
            if (prob == Integer.MIN_VALUE) {
                prob = 100;
            }
            // Init probability text field
            JTextField probField = new JTextField(new IntegerDocument(), String.valueOf(prob), 3);
            probField.setMinimumSize(new Dimension(40, 25));
            probField.setPreferredSize(new Dimension(40, 25));
            probField.setMaximumSize(new Dimension(40, 25));
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
            pedgePanel.setMinimumSize(new Dimension(320, 25));
            pedgePanel.setPreferredSize(new Dimension(320, 25));
            pedgePanel.setMaximumSize(new Dimension(320, 25));
            pedgePanel.setLayout(new BoxLayout(pedgePanel, BoxLayout.X_AXIS));
            pedgePanel.add(Box.createRigidArea(new Dimension(20, 0)));
            pedgePanel.add(pedgeDescription);
            pedgePanel.add(Box.createRigidArea(new Dimension(5, 0)));
            pedgePanel.add(Box.createHorizontalGlue());
            pedgePanel.add(probField);
            pedgePanel.add(Box.createRigidArea(new Dimension(10, 0)));
            mEdgeProbPanel.add(pedgePanel);
        }
        // Init rest panel
        mRestPanel = new JPanel();
        mRestPanel.setLayout(new BoxLayout(mRestPanel, BoxLayout.X_AXIS));
        mRestPanel.setMinimumSize(new Dimension(320, 25));
        mRestPanel.setPreferredSize(new Dimension(320, 25));
        mRestPanel.setMaximumSize(new Dimension(320, 25));
        //
        mRestLabel = new JLabel("Rest:");
        mRestLabel.setMinimumSize(new Dimension(240, 25));
        mRestLabel.setPreferredSize(new Dimension(240, 25));
        mRestLabel.setMaximumSize(new Dimension(240, 25));
        //
        mRestField = new JTextField(3);
        mRestField.setDocument(new IntegerDocument());
        mRestField.setEditable(false);
        mRestField.setEnabled(false);
        mRestField.setMinimumSize(new Dimension(40, 25));
        mRestField.setPreferredSize(new Dimension(40, 25));
        mRestField.setMaximumSize(new Dimension(40, 25));

        mRestPanel.add(Box.createRigidArea(new Dimension(20, 0)));
        mRestPanel.add(mRestLabel);
        mRestPanel.add(Box.createRigidArea(new Dimension(5, 0)));
        mRestPanel.add(Box.createHorizontalGlue());
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

    private void saveProbabilities() {
        if (!areProbabilitiesValid()) {
            return;
        }
        //Save the probabilities 
        Iterator it = mPEdgeMap.entrySet().iterator();
        while (it.hasNext()) {
            Map.Entry entry = (Map.Entry) it.next();
            PEdge edge = (PEdge) entry.getKey();
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
                    //ERROR
                } else {
                    sum += value;
                }
            } catch (NumberFormatException e) {
                //ERROR
            }
        }

        for (JTextField textField : mPEdgeMap.values()) {
            double prob = Integer.valueOf(textField.getText().trim()).doubleValue();
            double ratiuon = (prob / Integer.valueOf(sum).doubleValue()) * 100.0d;
            textField.setText(Integer.valueOf((int) Math.round(ratiuon)).toString());
        }

    }

    private void uniformActionPerformed() {
        int numEdges = mPEdgeMap.size();
        int uniProb = 100 / numEdges;
        int restVal = 100 % numEdges;

        for (JTextField textField : mPEdgeMap.values()) {
            textField.setText(Integer.toString(uniProb));
        }

//        for (JTextField textField : mPEdgeMap.values()) {
//
//            if (restVal == 0) {
//                break;
//            }
//            textField.setText(Integer.toString(Integer.valueOf(textField.getText().trim()) + 1));
//            restVal--;
//        }
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
