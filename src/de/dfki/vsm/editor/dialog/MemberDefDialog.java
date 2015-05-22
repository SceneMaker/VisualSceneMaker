package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JTextField;

/**
 * A dialog to create or edit a member definition.
 *
 * @author Gregor Mehlmann
 */
public class MemberDefDialog extends Dialog {

    // The member definition created or modified by this dialog
    private MemberDef mMemberDef;
    // GUI Components
    private JLabel mNameLabel;
    private JTextField mNameTextField;
    private JLabel mTypeLabel;
    private JComboBox mTypeComboBox;
    private OKButton mOkButton;
    private CancelButton mCancelButton;

    public MemberDefDialog(JDialog parent, MemberDef memberDef) {
        super(parent, "Member Definition", true);
        if (memberDef != null) {
            mMemberDef = memberDef.getCopy();
        } else {
            mMemberDef = new MemberDef("newMember", "Bool");
        }
        initComponents();
        fillComponents();
    }

    protected void initComponents() {
        mNameLabel = new JLabel("Name:");
        mTypeLabel = new JLabel("Type:");
        mNameTextField = new JTextField();
        mTypeComboBox = new JComboBox(new Object[]{"Bool", "Int", "Float", "String"});
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });
        addComponent(mNameLabel, 10, 20, 70, 30);
        addComponent(mNameTextField, 90, 20, 200, 30);
        addComponent(mTypeLabel, 10, 65, 70, 30);
        addComponent(mTypeComboBox, 90, 65, 200, 30);
        addComponent(mOkButton, 175, 120, 125, 30);
        addComponent(mCancelButton, 30, 120, 125, 30);
        packComponents(320, 170);
    }

    private void fillComponents() {
        mNameTextField.setText(mMemberDef.getName());
        mTypeComboBox.setSelectedItem(mMemberDef.getType());
    }

    public MemberDef run() {
        setVisible(true);
        if (mPressedButton == Button.OK) {
            return mMemberDef;
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
        mMemberDef.setName(mNameTextField.getText().trim());
        mMemberDef.setType((String) mTypeComboBox.getSelectedItem());
        return true;
    }
}
