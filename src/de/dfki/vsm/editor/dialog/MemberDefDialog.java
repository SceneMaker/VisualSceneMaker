package de.dfki.vsm.editor.dialog;

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
    private JButton mOkButton;
    private JButton mCancelButton;

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
        addCompoment(mNameLabel, 10, 10, 70, 20);
        addCompoment(mNameTextField, 90, 10, 200, 20);
        addCompoment(mTypeLabel, 10, 35, 70, 20);
        addCompoment(mTypeComboBox, 90, 35, 200, 20);
        addCompoment(mOkButton, 130, 60, 80, 20);
        addCompoment(mCancelButton, 210, 60, 80, 20);
        packComponents(300, 85);
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
