package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.condition.logical.LogicalCond;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.ios.ResourceLoader;
import de.dfki.vsm.util.tpl.TPLTuple;
import static java.awt.Component.CENTER_ALIGNMENT;
import static java.awt.Component.LEFT_ALIGNMENT;
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

/**
 *
 * @author Gregor Mehlmann
 */
public class ModifyCEdgeDialog extends Dialog {
  
    // The edge that we want to modify
    private final CEdge mCEdge;
    // GUI-Components
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

    public ModifyCEdgeDialog(CEdge cedge) {
        super(Editor.getInstance(), "Modify Conditional Edge", true);
        mCEdge = cedge;
        // TODO: move to EdgeDialog
        mAltStartNodeManager = new AltStartNodeManager(mCEdge);
        // Init GUI-Components
        initComponents();
        mInputTextField.setText(mCEdge.getCondition().getConcreteSyntax());
        loadAltStartNodeMap();
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
        
        // Input label
        mInputLabel = new JLabel("Conditional Expression:");
        // Input text field
        mInputTextField = new JTextField();
        
        panelContainer = new JPanel(null);
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
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        
        // Cancel button
        mCancelButton = new JButton("Cancel");    
        mCancelButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        
        // Button panel
        mButtonPanel = new JPanel(null);        
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
        mAltStartNodeList = new JList(new DefaultListModel());
        mAltStartNodeScrollPane = new JScrollPane(mAltStartNodeList);
        
        // Init alternative start node buttons300
        mAddAltStartNodeButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/plus.png"));  
        mAddAltStartNodeButton.setMaximumSize(new Dimension(20, 20));
        mAddAltStartNodeButton.setPreferredSize(new Dimension(20, 20));
	mAddAltStartNodeButton.setMinimumSize(new Dimension(20, 20)); 
        mAddAltStartNodeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                addAltStartNode();
            }
        });
        
        mRemoveAltStartNodeButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/minus.png"));
        mRemoveAltStartNodeButton.setMaximumSize(new Dimension(20, 20));
        mRemoveAltStartNodeButton.setPreferredSize(new Dimension(20, 20));
	mRemoveAltStartNodeButton.setMinimumSize(new Dimension(20, 20));  
        mRemoveAltStartNodeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                removeAltStartNode();
            }
        });
        
        mEditAltStartNodeButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/edit.png"));
        mEditAltStartNodeButton.setMaximumSize(new Dimension(20, 20));
        mEditAltStartNodeButton.setPreferredSize(new Dimension(20, 20));
	mEditAltStartNodeButton.setMinimumSize(new Dimension(20, 20)); 
        mEditAltStartNodeButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                editAltStartNode();
            }
        });      
        
        titleContainer = new JPanel(null);
        titleContainer.setLayout(new BoxLayout(titleContainer, BoxLayout.X_AXIS));
        titleContainer.setAlignmentX(LEFT_ALIGNMENT);
        titleContainer.add(mAltStartNodeLabel);
        titleContainer.add(Box.createRigidArea(new Dimension(1000, 20)));        
        
        buttonsContainer = new JPanel(null);
        buttonsContainer.setLayout(new BoxLayout(buttonsContainer, BoxLayout.Y_AXIS));   
        buttonsContainer.setMaximumSize(new Dimension(20, 60));
        buttonsContainer.add(mAddAltStartNodeButton);
        buttonsContainer.add(mRemoveAltStartNodeButton);
        buttonsContainer.add(mEditAltStartNodeButton);
        
        startNodeContainer = new JPanel(null);
        startNodeContainer.setLayout(new BoxLayout(startNodeContainer, BoxLayout.X_AXIS));
        startNodeContainer.add(Box.createRigidArea(new Dimension(3, 20)));
        startNodeContainer.add(mAltStartNodeScrollPane);        
        startNodeContainer.add(buttonsContainer);        
        startNodeContainer.add(Box.createRigidArea(new Dimension(3, 20)));
        
        // Init alternative start node panel
        mAltStartNodePanel = new JPanel(null);        
        mAltStartNodePanel.setLayout(new BoxLayout(mAltStartNodePanel, BoxLayout.PAGE_AXIS));
        mAltStartNodePanel.setAlignmentX(CENTER_ALIGNMENT);
        mAltStartNodePanel.add(titleContainer);   
        //cmAltStartNodePanel.add(Box.createRigidArea(new Dimension(5, 5)));
        mAltStartNodePanel.add(startNodeContainer);
    }
    
    
//    private void initComponents() {
//        mInputTextField = new JTextField();
//        mInputTextField.setBounds(10, 10, 300, 20);
//        mOkButton = new JButton("Ok");
//        mOkButton.setBounds(10, 35, 90, 20);
//        mOkButton.addActionListener(new ActionListener() {
//
//            public void actionPerformed(ActionEvent e) {
//                okActionPerformed();
//            }
//        });
//        mCancelButton = new JButton("Cancel");
//        mCancelButton.setBounds(100, 35, 90, 20);
//        mCancelButton.addActionListener(new ActionListener() {
//
//            public void actionPerformed(ActionEvent e) {
//                cancelActionPerformed();
//            }
//        });
//        addCompoment(mInputTextField, 10, 10, 300, 20);
//        addCompoment(mOkButton, 10, 35, 90, 20);
//        addCompoment(mCancelButton, 100, 35, 90, 20);
//        packComponents(320, 60);
//    }

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
            if (log != null && !_SFSLParser_.errorFlag) {
                mCEdge.setCondition(log);
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

        if (mCEdge.getTargetNode() instanceof SuperNode) {
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
    
    public JPanel getInputPanel(){
        return mInputPanel;
    }
    
    public JPanel getAltStartNodePanel(){
        return mAltStartNodePanel;
    }
    
    public JPanel getButtonPanel(){
        return mButtonPanel;
    }    
    
    public JTextField getInputTextField(){
        return mInputTextField;
    }
    
}
