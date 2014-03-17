package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
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
    private JButton mOkButton;
    private JButton mCancelButton;

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
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        addCompoment(mStartNodeLabel, 10, 10, 100, 20);
        addCompoment(mStartNodeComboBox, 120, 10, 200, 20);
        addCompoment(mAltStartNodeLabel, 10, 35, 100, 20);
        addCompoment(mAltStartNodeComboBox, 120, 35, 200, 20);
        addCompoment(mOkButton, 130, 125, 90, 20);
        addCompoment(mCancelButton, 220, 125, 90, 20);
        packComponents(320, 160);
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
