package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import javax.swing.Box;

//~--- JDK imports ------------------------------------------------------------

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
    private JLabel       mNameLabel;
    private JTextField   mNameTextField;
    private JLabel       mTypeLabel;
    private JComboBox    mTypeComboBox;
    private OKButton     mOkButton;
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
        mNameLabel     = new JLabel("Name:");
        mTypeLabel     = new JLabel("Type:");
        mNameTextField = new JTextField();
        mTypeComboBox  = new JComboBox(new Object[] { "Bool", "Int", "Float", "String" });
        
        //Name box
        Box nameBox = Box.createHorizontalBox();
        nameBox.add(mNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mNameTextField);        
        
        //Type box
        Box typeBox = Box.createHorizontalBox();
        typeBox.add(mTypeLabel);
        typeBox.add(Box.createHorizontalStrut(10));
        typeBox.add(mTypeComboBox);        
        
        mOkButton      = new OKButton();
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
        //Button box
        Box buttonBox = Box.createHorizontalBox();
        buttonBox.add(Box.createHorizontalGlue());
        buttonBox.add(mCancelButton);
        buttonBox.add(Box.createHorizontalStrut(10));
        buttonBox.add(mOkButton);
        
        Box finalBox = Box.createVerticalBox();
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(typeBox);
        finalBox.add(Box.createVerticalStrut(25));
        finalBox.add(buttonBox);
        addComponent(finalBox, 10, 20, 300, 150);
        packComponents(340, 180);
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
