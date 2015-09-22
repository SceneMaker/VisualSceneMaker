package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.BorderFactory;
import javax.swing.Box;

//~--- JDK imports ------------------------------------------------------------

import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;

/**
 * A dialog to create or edit a member definition.
 *
 * @author Not me
 */
public class MemberDefDialog extends Dialog {

    // The member definition created or modified by this dialog
    private MemberDef mMemberDef;

    // GUI Components
    private JLabel       mNameLabel;
    private HintTextField   mNameTextField;
    private JLabel       mTypeLabel;
    private JComboBox    mTypeComboBox;
    private OKButton     mOkButton;
    private CancelButton mCancelButton;
    private Dimension labelSize = new Dimension(75, 30);
    private Dimension textFielSize = new Dimension(175, 30);
    private JLabel errorMsg;
    
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
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mTypeLabel, labelSize);
        mNameTextField = new HintTextField("Enter Name");
        mTypeComboBox  = new JComboBox(new Object[] { "Bool", "Int", "Float", "String" });
        sanitizeComponent(mNameTextField, textFielSize);
        sanitizeComponent(mTypeComboBox, textFielSize);
        
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
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(labelSize);
        
        Box finalBox = Box.createVerticalBox();
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(typeBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(buttonBox);
        addComponent(finalBox, 10, 20, 300, 170);
        packComponents(340, 190);
        mOkButton.requestFocus();
    }

    private void fillComponents() {
        mNameTextField.setText(mMemberDef.getName());
        mTypeComboBox.setSelectedItem(mMemberDef.getType());
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
        if(mNameTextField.getText().length() == 0){
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);

            return false;
        }
        mMemberDef.setName(mNameTextField.getText().trim());
        mMemberDef.setType((String) mTypeComboBox.getSelectedItem());

        return true;
    }
}
