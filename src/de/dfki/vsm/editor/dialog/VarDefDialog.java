package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

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
    private final Node   mNode;
    private final VarDef mVarDef;

    // GUI Components
    private JLabel               mNameLabel;
    private JLabel               mTypeDefLabel;
    private JLabel               mExpLabel;
    private JTextField           mNameTextField;
    private JTextField           mExpTextField;
    private JButton              mAddExpButton;
    private JComboBox            mTypeDefComboBox;
    private DefaultComboBoxModel mTypeDefComboBoxModel;
    private OKButton             mOkButton;
    private CancelButton         mCancelButton;

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
        mNameLabel     = new JLabel("Name:");
        mNameTextField = new JTextField(mVarDef.getName());

        //
        mExpLabel     = new JLabel("Value:");
        mExpTextField = new JTextField();
        mExpTextField.setEditable(false);

        //
        mTypeDefLabel         = new JLabel("Type:");
        mTypeDefComboBoxModel = new DefaultComboBoxModel();
        mTypeDefComboBox      = new JComboBox(mTypeDefComboBoxModel);

        //
        mAddExpButton = new JButton(ResourceLoader.loadImageIcon("/res/img/search_icon.png"));
        mAddExpButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/search_icon_blue.png"));
        mAddExpButton.setOpaque(false);
        mAddExpButton.setContentAreaFilled(false);
        mAddExpButton.setFocusable(false);
        mAddExpButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                selectExp();
            }
        });

        //
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });

        //
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });

        //
        addCompoment(mNameLabel, 10, 20, 70, 30);
        addCompoment(mNameTextField, 90, 20, 260, 30);
        addCompoment(mTypeDefLabel, 10, 85, 70, 30);
        addCompoment(mTypeDefComboBox, 90, 85, 260, 30);
        addCompoment(mExpLabel, 10, 150, 70, 30);
        addCompoment(mExpTextField, 90, 150, 230, 30);
        addCompoment(mAddExpButton, 320, 150, 30, 30);
        addCompoment(mCancelButton, 75, 250, 125, 30);
        addCompoment(mOkButton, 225, 250, 125, 30);
        packComponents(400, 300);
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
