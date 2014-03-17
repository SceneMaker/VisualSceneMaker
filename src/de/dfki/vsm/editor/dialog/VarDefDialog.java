package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JTextField;

/**
 * @author Gregor Mehlmann
 */
public class VarDefDialog extends Dialog {

    private final Node mNode;
    private final VarDef mVarDef;
    // GUI Components
    private JLabel mNameLabel;
    private JLabel mTypeDefLabel;
    private JLabel mExpLabel;
    private JTextField mNameTextField;
    private JTextField mExpTextField;
    private JButton mAddExpButton;
    private JComboBox mTypeDefComboBox;
    private DefaultComboBoxModel mTypeDefComboBoxModel;
    private JButton mOkButton;
    private JButton mCancelButton;

    public VarDefDialog(Node node, VarDef varDef) {
        super(Editor.getInstance(), "Create/Modify Variable Definition", true);
        mNode = node;
        if (varDef != null) {
            mVarDef = varDef.getCopy();
        } else {
            mVarDef = new VarDef("newVar", "Bool", new Bool(true));
        }
        initComponents();
        fillComponents();
    }

    private void initComponents() {
        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new JTextField(mVarDef.getName());
        //
        mExpLabel = new JLabel("Value:");
        mExpTextField = new JTextField();
        mExpTextField.setEditable(false);
        //
        mTypeDefLabel = new JLabel("Type:");
        mTypeDefComboBoxModel = new DefaultComboBoxModel();
        mTypeDefComboBox = new JComboBox(mTypeDefComboBoxModel);
        //
        mAddExpButton = new JButton("...");
        mAddExpButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                selectExp();
            }
        });
        //
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        //
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {

            public void actionPerformed(ActionEvent e) {
                cancelActionPerformed();
            }
        });
        //
        addCompoment(mNameLabel, 10, 10, 70, 20);
        addCompoment(mNameTextField, 90, 10, 200, 20);
        addCompoment(mTypeDefLabel, 10, 35, 70, 20);
        addCompoment(mTypeDefComboBox, 90, 35, 200, 20);
        addCompoment(mExpLabel, 10, 60, 70, 20);
        addCompoment(mExpTextField, 90, 60, 180, 20);
        addCompoment(mAddExpButton, 270, 60, 20, 20);
        addCompoment(mOkButton, 130, 185, 80, 20);
        addCompoment(mCancelButton, 210, 185, 80, 20);
        packComponents(300, 210);
    }

    private void fillComponents() {
        // Show the basic built-in types
        mTypeDefComboBoxModel.addElement("Int");
        mTypeDefComboBoxModel.addElement("Bool");
        mTypeDefComboBoxModel.addElement("Float");
        mTypeDefComboBoxModel.addElement("String");
        mTypeDefComboBoxModel.addElement("Object");
        // Show the type definitions of the current node modification status.
        for (TypeDef def : mNode.getTypeDefList()) {
            mTypeDefComboBoxModel.addElement(def.getName());
        }
        // Show the type definitions of all parent nodes of the current node
        SuperNode parentNode = mNode.getParentNode();
        while (parentNode != null) {
            for (TypeDef def : parentNode.getTypeDefList()) {
                mTypeDefComboBoxModel.addElement(def.getName());
            }
            parentNode = parentNode.getParentNode();
        }
        // Select the type of the variable definition
        mTypeDefComboBox.setSelectedItem(mVarDef.getType());
        // Select the expression of the variable definition
        if (mVarDef.getExp() != null) {
            mExpTextField.setText(mVarDef.getExp().getAbstractSyntax());
        }
    }

    private void selectExp() {
        Expression exp = new CreateExpDialog(null).run();
        if (exp != null) {
            mVarDef.setExp(exp);
            mExpTextField.setText(exp.getAbstractSyntax());
        }
    }

    public VarDef run() {
        setVisible(true);
        if (mPressedButton == Button.OK) {
            return mVarDef;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        mVarDef.setName(mNameTextField.getText().trim());
        mVarDef.setType((String) mTypeDefComboBoxModel.getSelectedItem());
        dispose(Button.OK);
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }
}
