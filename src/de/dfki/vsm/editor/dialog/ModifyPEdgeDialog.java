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
import de.dfki.vsm.model.sceneflow.BasicNode;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.tpl.TPLTuple;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.MouseEvent;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class ModifyPEdgeDialog extends Dialog {

    //
    private HashMap<PEdge, JTextField> mPEdgeMap = new HashMap<PEdge, JTextField>();
    private final Dimension buttonSize = new Dimension(150, 30);
    private final Dimension smallButtonSize = new Dimension(30, 30);
    // Data model components
    private final BasicNode mSourceNode;
    private final BasicNode mTargetNode;
    private final PEdge mPEdge;

    //
    private final AltStartNodeManager mAltStartNodeManager;

    // GUI-Components
    private JPanel mHeaderPanel;
    private JLabel mHeaderLabel;

    //
    private JPanel mButtonPanel;
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private JLabel mNormButton;
    private JLabel mUniButton;

    //
    private JPanel mAltStartNodePanel;
    private JLabel mAltStartNodeLabel;
    private JList mAltStartNodeList;
    private JScrollPane mAltStartNodeScrollPane;
    private AddButton mAddAltStartNodeButton;
    private RemoveButton mRemoveAltStartNodeButton;
    private EditButton mEditAltStartNodeButton;

    //
    private JPanel mEdgeProbPanel;

    //
    private JPanel mRestPanel;
    private JLabel mRestLabel;
    private JTextField mRestField;
    private Dimension labelSize = new Dimension(200, 30);
    private Dimension textFielSize = new Dimension(230, 30);
    private JLabel errorMsg;
    
    //ICONS
    private final ImageIcon ICON_NORMALIZE_STANDARD = ResourceLoader.loadImageIcon("/res/img/normalize_gray.png");
    private final ImageIcon ICON_NORMALIZE_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/normalize_blue.png");
    
    private final ImageIcon ICON_UNIFORM_STANDARD = ResourceLoader.loadImageIcon("/res/img/uniform_gray.png");
    private final ImageIcon ICON_UNIFORM_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/uniform_blue.png");
    
    public ModifyPEdgeDialog(BasicNode sourceNode, BasicNode targetNode) {
        super(EditorInstance.getInstance(), "Create Probability Edge", true);
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

    public ModifyPEdgeDialog(PEdge edge) {
        super(EditorInstance.getInstance(), "Modify Probability Edge", true);
        mPEdge = edge;
        mSourceNode = mPEdge.getSourceNode();
        mTargetNode = mPEdge.getTargetNode();

        // Fill the map with the data
        for (PEdge pedge : mSourceNode.getPEdgeList()) {
            mPEdgeMap.put(pedge, /* new JTextField("", 3) */ null);
        }

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mPEdge);

        // Init the GUI-Components
        initComponents();

        //
        loadAltStartNodeMap();
    }

    /**
     * Set the correct size of the components
     *
     * @param jb
     * @param dim
     */
    private void sanitizeComponent(JComponent jb, Dimension dim) {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }

    private void initComponents() {

        // Init probability panel
        initEdgeProbabilityPanel();

        // Init button panel
        initButtonPanel();

        // Init alternative start node panel
        initAltStartNodePanel();
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(labelSize);
        //FINAL 
        Box finalBox = Box.createVerticalBox();
        finalBox.add(mEdgeProbPanel);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mAltStartNodePanel);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mButtonPanel);

        addComponent(finalBox, 20, 20, 480, 480);

        packComponents(510, 500);
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
        Box secondButtonBox = Box.createHorizontalBox();
        secondButtonBox.add(Box.createHorizontalGlue());
        secondButtonBox.add(mCancelButton);
        secondButtonBox.add(Box.createHorizontalStrut(30));
        secondButtonBox.add(mOkButton);
        secondButtonBox.add(Box.createHorizontalGlue());

        mButtonPanel = new JPanel(null);
        mButtonPanel.setOpaque(false);
        mButtonPanel.setMinimumSize(new Dimension(480, 80));
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.Y_AXIS));
//        mButtonPanel.add(firstButtonBox);
//        mButtonPanel.add(Box.createVerticalStrut(30));
        mButtonPanel.add(secondButtonBox);

    }

    private void initEdgeProbabilityPanel() {

        // Init header label
        mHeaderLabel = new JLabel("Edge Probabilities:");
        sanitizeComponent(mHeaderLabel, labelSize);

        // Init edge probability panel
        mEdgeProbPanel = new JPanel();
        mEdgeProbPanel.setOpaque(false);
        mEdgeProbPanel.setLayout(new BoxLayout(mEdgeProbPanel, BoxLayout.Y_AXIS));
        mEdgeProbPanel.add(mHeaderLabel);
        mEdgeProbPanel.add(Box.createVerticalStrut(30));
        //nodes scroll pane
        JPanel nodesPanel = new JPanel();
        nodesPanel.setLayout(new BoxLayout(nodesPanel, BoxLayout.Y_AXIS));
        nodesPanel.setBorder(null);
        JScrollPane nodesScrollPanel = new JScrollPane(nodesPanel);
        nodesScrollPanel.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        //nodesScrollPanel.setPreferredSize(new Dimension(480, 90));
        nodesScrollPanel.setMinimumSize(new Dimension(340, 90));
        nodesScrollPanel.setMaximumSize(new Dimension(480, 90));
        nodesScrollPanel.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        nodesScrollPanel.setBorder(null);
        //
        boolean requiresUniformAction = false;
        
        for (PEdge pedge : mPEdgeMap.keySet()) {

            // Init description label
            JLabel pedgeDescription = new JLabel(mSourceNode.getName() + " ( " + mSourceNode.getId() + " ) "
                    + " \u2192  " + pedge.getTargetNode().getName() + " ( "
                    + pedge.getTargetNode().getId() + " ) ");

            sanitizeComponent(pedgeDescription, labelSize);
            // Compute initial probability
            int prob = pedge.getProbability();

            if (prob == Integer.MIN_VALUE) {
                prob = 100;
                if (mPEdgeMap.size() >= 3)
                {
                    prob = 0;
                }
                else 
                {
                    requiresUniformAction = true;
                }
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
                        }
                        catch (NumberFormatException es) {
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
            pedgePanel.setLayout(new BoxLayout(pedgePanel, BoxLayout.X_AXIS));
            pedgePanel.add(pedgeDescription);
            pedgePanel.add(Box.createRigidArea(new Dimension(15, 0)));
            pedgePanel.add(probField);
            pedgePanel.add(Box.createHorizontalStrut(30));
            nodesPanel.add(pedgePanel);
//            mEdgeProbPanel

        }
        
        // Init rest panel
        mRestPanel = new JPanel();
        mRestPanel.setOpaque(false);
        mRestPanel.setLayout(new BoxLayout(mRestPanel, BoxLayout.X_AXIS));
        mRestLabel = new JLabel("Rest:");
        sanitizeComponent(mRestLabel, labelSize);
        mRestField = new JTextField(3);
        mRestField.setMinimumSize(new Dimension(40, 25));
        mRestField.setPreferredSize(new Dimension(40, 25));
        mRestField.setMaximumSize(new Dimension(40, 25));
        mRestField.setDocument(new IntegerDocument());
        mRestField.setEditable(false);
        mRestField.setEnabled(false);
        mRestPanel.add(mRestLabel);
        mRestPanel.add(Box.createRigidArea(new Dimension(15, 0)));
        mRestPanel.add(mRestField);
        mRestPanel.add(Box.createHorizontalGlue());
        
        // Normalize button
        mNormButton = new JLabel();
        mNormButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mNormButton.setOpaque(true);
        mNormButton.setBackground(Color.white);
        mNormButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mNormButton.setToolTipText("Normalize");
        mNormButton.setIcon(ICON_NORMALIZE_STANDARD);
       // mNormButton.setIconTextGap(20);
        //mNormButton.setFont(new Font("Helvetica", Font.PLAIN, 20));
        mNormButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mNormButton.setPreferredSize(smallButtonSize);
        mNormButton.setMinimumSize(smallButtonSize);
        mNormButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                normalizeActionPerformed();
            }

            public void mouseEntered(MouseEvent me) {
                mNormButton.setIcon(ICON_NORMALIZE_ROLLOVER);
                mNormButton.setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                mNormButton.setIcon(ICON_NORMALIZE_STANDARD);
                mNormButton.setBackground(new Color(255, 255, 255));
            }
        });

        // Uniform button
        mUniButton = new JLabel();
        mUniButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mUniButton.setOpaque(true);
        mUniButton.setBackground(Color.white);
        mUniButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mUniButton.setToolTipText("Uniform");
        mUniButton.setIcon(ICON_UNIFORM_STANDARD);
        //mUniButton.setIconTextGap(20);
        //mUniButton.setFont(new Font("Helvetica", Font.PLAIN, 20));
        mUniButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mUniButton.setPreferredSize(smallButtonSize);
        mUniButton.setMinimumSize(smallButtonSize);
        mUniButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                uniformActionPerformed();
            }

            public void mouseEntered(MouseEvent me) {
                mUniButton.setIcon(ICON_UNIFORM_ROLLOVER);
                mUniButton.setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                mUniButton.setIcon(ICON_UNIFORM_STANDARD);
                mUniButton.setBackground(new Color(255, 255, 255));
            }
        });
         // Button panel
        Box topContainer = Box.createHorizontalBox();
        Box probButtonsBox = Box.createVerticalBox();
       // probButtonsBox.add(Box.createHorizontalGlue());
        probButtonsBox.add(mNormButton);
        probButtonsBox.add(Box.createVerticalStrut(20));
        probButtonsBox.add(mUniButton);
       // probButtonsBox.add(Box.createHorizontalGlue());
        topContainer.add(nodesScrollPanel);
        topContainer.add(Box.createHorizontalStrut(130));
        topContainer.add(probButtonsBox);
        // Add the rest panel
        mEdgeProbPanel.add(topContainer);
        mEdgeProbPanel.add(Box.createVerticalStrut(40));
        mEdgeProbPanel.add(mRestPanel);

        // Highlight the prbability text field of the current edge
        mPEdgeMap.get(mPEdge).setCaretPosition(0);
        if(requiresUniformAction)
        {
            uniformActionPerformed();
        }
    }

    protected void initAltStartNodePanel() {

        // Init alternative start node label
        mAltStartNodeLabel = new JLabel("Alternative Start Nodes:");
        sanitizeComponent(mAltStartNodeLabel, labelSize);
        // Init alternative start node list
        mAltStartNodeList = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        Dimension tfSize = new Dimension(200, 110);
        mAltStartNodeScrollPane.setPreferredSize(tfSize);
        mAltStartNodeScrollPane.setMinimumSize(tfSize);
        mAltStartNodeScrollPane.setMaximumSize(tfSize);

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
        mAltStartNodePanel.add(Box.createHorizontalGlue());
    }

    private void saveProbabilities() {
        if (!areProbabilitiesValid()) {
            return;
        }

        // Save the probabilities
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
                if (textField.getText().length() == 0) {
                    textField.setBorder(BorderFactory.createLineBorder(Color.red));
                    errorMsg.setForeground(Color.red);
                    return false;
                }
                sum += Integer.valueOf(textField.getText().trim()).intValue();
            } catch (NumberFormatException e) {
                return false;
            }
        }

        return (sum == 100);
    }

    public PEdge run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mPEdge;
        } else {
            return null;
        }
    }

    public void normalizeActionPerformed() {

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
            double prob = Integer.valueOf(textField.getText().trim()).doubleValue();
            double ratiuon = (prob / Integer.valueOf(sum).doubleValue()) * 100.0d;

            textField.setText(Integer.valueOf((int) Math.round(ratiuon)).toString());
        }
    }

    public void uniformActionPerformed() {
        int numEdges = mPEdgeMap.size();
        int uniProb = 100 / numEdges;
        int restVal = 100 % numEdges;

        for (JTextField textField : mPEdgeMap.values()) {
            textField.setText(Integer.toString(uniProb));
        }
    }

    @Override
    public void okActionPerformed() {
        if (areProbabilitiesValid()) {
            saveProbabilities();
            saveAltStartNodeMap();
            dispose(Button.OK);
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mPEdge.getTargetNode() instanceof SuperNode) {
            Iterator it = mAltStartNodeManager.mAltStartNodeMap.entrySet().iterator();

            while (it.hasNext()) {
                Map.Entry pairs = (Map.Entry) it.next();
                TPLTuple<String, BasicNode> startNodePair = (TPLTuple<String, BasicNode>) pairs.getKey();
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
            Map.Entry pairs = (Map.Entry) it.next();
            TPLTuple<String, BasicNode> startNodePair = (TPLTuple<String, BasicNode>) pairs.getKey();
            TPLTuple<String, BasicNode> altStartNodePair = (TPLTuple<String, BasicNode>) pairs.getValue();

            ((DefaultListModel) mAltStartNodeList.getModel()).addElement(startNodePair.getFirst() + "/"
                    + altStartNodePair.getFirst());
        }
    }

    private void removeAltStartNode() {
        String selectedValue = (String) mAltStartNodeList.getSelectedValue();

        if (selectedValue != null) {
            String[] idPair = selectedValue.split("/");
            String startNodeId = idPair[0];

            // String altStartNodeId = idPair[1];
            System.err.println("remove alt start node" + startNodeId);
            mAltStartNodeManager.removeAltStartNode(startNodeId);
            ((DefaultListModel) mAltStartNodeList.getModel()).removeElement(selectedValue);
        }
    }

    private void editAltStartNode() {
    }

    public JPanel getEdgeProbPanel() {
        return mEdgeProbPanel;
    }

    public JPanel getButtonPanel() {
        return mButtonPanel;
    }

    public JLabel getNormButton() {
        return mNormButton;
    }

    public JLabel getUniButton() {
        return mUniButton;
    }

    public JPanel getAltStartNodePanel() {
        return mAltStartNodePanel;
    }

    public HashMap<PEdge, JTextField> getPEdgeMap() {
        return mPEdgeMap;
    }
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
