package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.event.CEdgeDialogModifiedEvent;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.glue.GlueParser;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.tpl.TPLTuple;
import java.awt.Color;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Iterator;
import java.util.Map;

import javax.swing.*;

import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Gregor Mehlmann
 */
public class ModifyCEdgeDialog extends Dialog implements EventListener{

    // The edge that we want to modify
    private final GuargedEdge mCEdge;

    // GUI-Components
    private final AltStartNodeManager mAltStartNodeManager;
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();
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
    private DocumentListener docListener;

    public ModifyCEdgeDialog(BasicNode sourceNode, BasicNode targetNode) {
        super(EditorInstance.getInstance(), "Create Conditional Edge", true);
        // Set the edge data
        mCEdge = new GuargedEdge();
        mCEdge.setTargetUnid(targetNode.getId());
        mCEdge.setSourceNode(sourceNode);
        mCEdge.setTargetNode(targetNode);
        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mCEdge);
        // Init GUI-Components
        initComponents();
        initEvents();
    }
    public ModifyCEdgeDialog(GuargedEdge cedge) {
        super(EditorInstance.getInstance(), "Modify Conditional Edge", true);
        mCEdge = cedge;

        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mCEdge);

        // Init GUI-Components
        initComponents();
        mInputTextField.setText(mCEdge.getCondition().getConcreteSyntax());

        loadAltStartNodeMap();
        initEvents();
    }

    private void initEvents(){
        initDocListener();
        mEventCaster.register(this);
    }

    private void initComponents() {

        // Init input panel
        initInputPanel();

        // Init button panel
        initButtonPanel();
        // Init alternative start node panel
        initAltStartNodePanel();
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(labelSize);
        //Key listener need to gain focus on the text field

        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {

            @Override
            public boolean dispatchKeyEvent(KeyEvent ke) {
                if (ke.getID() == KeyEvent.KEY_PRESSED) {
                    if(!mInputTextField.hasFocus())
                    {
                        mInputTextField.requestFocus();
                    }
                }
                return false;
            }
        });
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

    private void initDocListener() {
        docListener = new MyDocumentListener();
        mInputTextField.getDocument().addDocumentListener( docListener);
    }



    private void initInputPanel() {
        // Input label
        mInputLabel = new JLabel("Conditional Expression:");
        sanitizeComponent(mInputLabel, labelSize);
        // Input text field
        mInputTextField = new HintTextField("(a < b )");
        
        
        //mInputTextField.tr();
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

    public GuargedEdge run() {
        setVisible(true);

        if (mPressedButton == Dialog.Button.OK) {
            return mCEdge;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (process()) {
            dispose(Dialog.Button.OK);
        }
        else{
            mInputTextField.setForeground(Color.red);
            EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().setMessageLabelText("Remember to wrap condition in parenthesis");
        }
        removeListener();
    }

    public void removeListener() {
        mEventCaster.remove(this);
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Dialog.Button.CANCEL);
        removeListener();
    }

    private boolean process() {
        if(mInputTextField.getText().length() == 0){
            mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
            return false;
        }
        String inputString = mInputTextField.getText().trim();

        try {
            final Expression exp = (Expression)  GlueParser.run(inputString);

            //LogicalCond log = ChartParser.logResult;
            //Expression log = ChartParser.expResult;

            if (exp != null) {
                mCEdge.setCondition(exp);
                mAltStartNodeManager.saveAltStartNodeMap();
                return true;
            } else {
                return false;
            }
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private void loadAltStartNodeMap() {
        mAltStartNodeManager.loadAltStartNodeMap();

        if (mCEdge.getTargetNode() instanceof SuperNode) {
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

    public JPanel getInputPanel() {
        return mInputPanel;
    }

    public JPanel getAltStartNodePanel() {
        return mAltStartNodePanel;
    }

    public JPanel getButtonPanel() {
        return mButtonPanel;
    }

    public HintTextField getInputTextField() {
        return mInputTextField;
    }

    @Override
    public void update(EventObject event) {
        if(event instanceof CEdgeDialogModifiedEvent && event.getSource() != this){
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    changeDuplicatedInputFieldText((CEdgeDialogModifiedEvent) event);
                }
            });

        }
    }

    private void changeDuplicatedInputFieldText(CEdgeDialogModifiedEvent event) {
        String text = event.getText();
        if(!text.equals(mInputTextField.getText())){
            mInputTextField.getDocument().removeDocumentListener(docListener); //Remove it first, so its not fired again creating an infinite loop
            mInputTextField.setText(text);
            mInputTextField.getDocument().addDocumentListener(docListener); //Add the listener again
        }
    }



    private class MyDocumentListener implements DocumentListener {
        @Override
        public void insertUpdate(DocumentEvent e) {
            fireEvent();
        }

        private void fireEvent() {
            mEventCaster.convey(new CEdgeDialogModifiedEvent(this, mInputTextField.getText()));
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
            fireEvent();
        }

        @Override
        public void changedUpdate(DocumentEvent e) {
            fireEvent();
        }
    }
}
