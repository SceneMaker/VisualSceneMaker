package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.AltStartNodeManager;
import de.dfki.vsm.model.sceneflow.Node;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;

/**
 * @author Gregor Mehlmann
 */
public class CreateAltStartNodeDialog extends Dialog {

    final private AltStartNodeManager mAltStartNodeManager;
    // GUI-Components
    private JLabel mStartNodeLabel;
    private JLabel mAltStartNodeLabel;
    private JComboBox mStartNodeComboBox;
    private JComboBox mAltStartNodeComboBox;
    private OKButton mOkButton;
    private CancelButton mCancelButton;

    public CreateAltStartNodeDialog(AltStartNodeManager manager) {
        super(Editor.getInstance(), "Create alternative Startnode", true);
        mAltStartNodeManager = manager;
        initComponents();
        loadSubstitutableStartNodes();
    }

    private void initComponents() {
        mStartNodeLabel = new JLabel("Start Node:");
        mStartNodeComboBox = new JComboBox(new DefaultComboBoxModel());
        mStartNodeComboBox.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                select();
            }
        });

        mAltStartNodeLabel = new JLabel("Alternative:");
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
        addCompoment(mStartNodeLabel, 10, 20, 100, 30);
        addCompoment(mStartNodeComboBox, 120, 20, 200, 30);
        addCompoment(mAltStartNodeLabel, 10, 60, 100, 30);
        addCompoment(mAltStartNodeComboBox, 120, 60, 200, 30);
        addCompoment(mOkButton, 175, 150, 125, 30);
        addCompoment(mCancelButton, 30, 150, 125, 30);
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
