package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.Node;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Vector;
import javax.swing.Box;
import javax.swing.BoxLayout;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author Not me
 */
public class CreateAltStartNodeDialog extends Dialog {
    final private AltStartNodeManager mAltStartNodeManager;

    // GUI-Components
    private JLabel       mStartNodeLabel;
    private JLabel       mAltStartNodeLabel;
    private JComboBox    mStartNodeComboBox;
    private JComboBox    mAltStartNodeComboBox;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;

    public CreateAltStartNodeDialog(AltStartNodeManager manager) {
        super(EditorInstance.getInstance(), "Create alternative Startnode", true);
        mAltStartNodeManager = manager;
        initComponents();
        loadSubstitutableStartNodes();
    }

    private void initComponents() {
        mStartNodeLabel    = new JLabel("Start Node:");
        mStartNodeComboBox = new JComboBox(new DefaultComboBoxModel());
        mStartNodeComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                select();
            }
        });
        mAltStartNodeLabel    = new JLabel("Alternative:");
        mAltStartNodeComboBox = new JComboBox(new DefaultComboBoxModel());

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
        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));
        
        // Start Node panel
        JPanel mStartNodePanel = new JPanel();
        mStartNodePanel.setLayout(new BoxLayout(mStartNodePanel, BoxLayout.X_AXIS));
        mStartNodePanel.add(mStartNodeLabel);
        mStartNodePanel.add(Box.createHorizontalStrut(10));
        mStartNodePanel.add(mStartNodeComboBox);
        
        // Alternative Start Node panel
        JPanel mAltStartNodePanel = new JPanel();
        mAltStartNodePanel.setLayout(new BoxLayout(mAltStartNodePanel, BoxLayout.X_AXIS));
        mAltStartNodePanel.add(mAltStartNodeLabel);
        mAltStartNodePanel.add(Box.createHorizontalStrut(10));
        mAltStartNodePanel.add(mAltStartNodeComboBox);
        
        Box finalBox = Box.createVerticalBox();
        finalBox.add(mStartNodePanel);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(mAltStartNodePanel);
        finalBox.add(Box.createVerticalStrut(30));
        finalBox.add(mButtonPanel);
        
        addComponent(finalBox, 10, 10, 320, 180);
        packComponents(340, 200);
    }

    public Object run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return null;
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
        String s = (String) mStartNodeComboBox.getSelectedItem();
        String a = (String) mAltStartNodeComboBox.getSelectedItem();

        if (a != null) {
            mAltStartNodeManager.createAltStartNode(s, a);
        }

        return true;
    }

    private void loadSubstitutableStartNodes() {

        // Create the nodes to select as  start nodes
        Vector<Node> substitutableStartNodeList = mAltStartNodeManager.getSubstitutableStartNodes();

        ((DefaultComboBoxModel) mStartNodeComboBox.getModel()).addElement("none");

        for (Node node : substitutableStartNodeList) {
            ((DefaultComboBoxModel) mStartNodeComboBox.getModel()).addElement(node.getId());
        }
    }

    private void select() {
        String id = (String) mStartNodeComboBox.getSelectedItem();

        System.err.println("Selected id=" + id);
        mAltStartNodeComboBox.removeAllItems();

        for (Node node : mAltStartNodeManager.getValidAltStartNodesFor(id)) {
            ((DefaultComboBoxModel) mAltStartNodeComboBox.getModel()).addElement(node.getId());
        }
    }
}
