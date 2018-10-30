package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.chart.BasicNode;
import de.dfki.vsm.model.sceneflow.chart.SuperNode;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.glue.command.definition.VariableDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.ListTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.datatype.StructTypeDefinition;
import de.dfki.vsm.model.sceneflow.glue.command.definition.DataTypeDefinition;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author Gregor Mehlmann
 */
public class VarDefDialog extends Dialog {

    private final BasicNode mNode;
    private VariableDefinition mVarDef;

    // GUI Components
    private JLabel mNameLabel;
    private JLabel mTypeDefLabel;
    private JLabel mExpLabel;
    private HintTextField mNameTextField;
    private HintTextField mExpTextField;
    private JButton mAddExpButton;
    private JComboBox mTypeDefComboBox;
    private DefaultComboBoxModel mTypeDefComboBoxModel;
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private Dimension labelSize = new Dimension(75, 30);
    private Dimension textFielSize = new Dimension(250, 30);
    private JLabel errorMsg;
    private boolean isNewVariable = true;

    public VarDefDialog(BasicNode node, VariableDefinition varDef) {
        super(EditorInstance.getInstance(), "Create/Modify Variable Definition", true);
        mNode = node;

        if (varDef != null) {
            mVarDef = varDef.getCopy();
            isNewVariable = false;
        } else {
            isNewVariable = true;
            mVarDef = new VariableDefinition("NewVar", "Bool", new BoolLiteral(true));
        }

        initComponents();
        fillComponents();
    }

    private void initComponents() {

        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new HintTextField("Enter Name");
        if (!mVarDef.getName().equals("NewVar")) {
            mNameTextField.setText(mVarDef.getName());
        }
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);
        //Name box
        Box nameBox = Box.createHorizontalBox();
        Box erroBox = Box.createHorizontalBox();
        nameBox.add(mNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mNameTextField);

        //
        mTypeDefLabel = new JLabel("Type:");
        mTypeDefComboBoxModel = new DefaultComboBoxModel();
        mTypeDefComboBox = new JComboBox(mTypeDefComboBoxModel);

        sanitizeComponent(mTypeDefLabel, labelSize);
        sanitizeComponent(mTypeDefComboBox, textFielSize);
        //Exp box
        Box typeDefBox = Box.createHorizontalBox();
        typeDefBox.add(mTypeDefLabel);
        typeDefBox.add(Box.createHorizontalStrut(10));
        typeDefBox.add(mTypeDefComboBox);

        //
        mExpLabel = new JLabel("Value:");
        mExpTextField = new HintTextField("Enter Value");
        mExpTextField.setEditable(false);
        mAddExpButton = new JButton(ResourceLoader.loadImageIcon("/res/img/search_icon.png"));
        mAddExpButton.setRolloverIcon(ResourceLoader.loadImageIcon("/res/img/search_icon_blue.png"));
        mAddExpButton.setBorder(null);
        mAddExpButton.setOpaque(false);
        mAddExpButton.setContentAreaFilled(false);
        mAddExpButton.setFocusable(false);
        mAddExpButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                selectExp();
            }
        });
        sanitizeComponent(mExpLabel, labelSize);
        sanitizeComponent(mExpTextField, new Dimension(200, 30));
        //Exp box
        Box expBox = Box.createHorizontalBox();
        expBox.add(mExpLabel);
        expBox.add(Box.createHorizontalStrut(10));
        expBox.add(mExpTextField);
        expBox.add(Box.createHorizontalStrut(20));
        expBox.add(mAddExpButton);

        mTypeDefComboBox.addItemListener(new ItemListener() {

            @Override
            @SuppressWarnings("empty-statement")
            public void itemStateChanged(ItemEvent e) {

                switch ((String) e.getItem()) {
                    case "Int":
                        mVarDef = new VariableDefinition("NewVar", "Int", new IntLiteral(0));
                        break;
                    case "Bool":
                        mVarDef = new VariableDefinition("NewVar", "Bool", new BoolLiteral(true));
                        break;
                    case "Float":
                        mVarDef = new VariableDefinition("NewVar", "Float", new de.dfki.vsm.model.sceneflow.glue.command.expression.literal.FloatLiteral(0));
                        break;
                    case "String":
                        mVarDef = new VariableDefinition("NewVar", "String", new de.dfki.vsm.model.sceneflow.glue.command.expression.literal.StringLiteral(""));
                        break;
                    default:
                        String a = "";
                        // Look in the definition list for the data type
                        for (DataTypeDefinition def : mNode.getTypeDefList()) {
                            if (def.getName().equals((String) e.getItem())) {
                                if (def instanceof ListTypeDefinition) {
                                    mVarDef = new VariableDefinition("NewVar", "List", new de.dfki.vsm.model.sceneflow.glue.command.expression.record.ArrayExpression());
                                } else if (def instanceof StructTypeDefinition) {
                                    mVarDef = new VariableDefinition("NewVar", "Struct", new de.dfki.vsm.model.sceneflow.glue.command.expression.record.StructExpression());
                                }
                            }
                        }
                        break;
                }
                mExpTextField.setText(mVarDef.getExp().getAbstractSyntax());
            }
        });

        //
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });
        //
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                cancelActionPerformed();
            }
        });
        // Button panel
        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.setBackground(Color.WHITE);
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));

        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        sanitizeComponent(errorMsg, new Dimension(400, 30));
        erroBox.add(errorMsg);

        //Key listener need to gain focus on the text field
        KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {

            @Override
            public boolean dispatchKeyEvent(KeyEvent ke) {
                //boolean keyHandled = false;
                if (ke.getID() == KeyEvent.KEY_PRESSED) {
                    if (!mNameTextField.hasFocus()) {
                        //mNameTextField.setText(mNameTextField.getText()+ke.getKeyChar());
                        mNameTextField.requestFocus();
                    }
                }
                return false;
            }
        });
        //FINAL BOX
        Box finalBox = Box.createVerticalBox();
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(typeDefBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(expBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(erroBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(mButtonPanel);
        addComponent(finalBox, 10, 20, 400, 290);
        packComponents(420, 300);
        mOkButton.requestFocus();
    }

    //
    private void sanitizeComponent(JComponent jb, Dimension dim) {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }

    private void fillComponents() {

        VariableDefinition varDefCopy = mVarDef.getCopy();
        // Show the basic built-in types
        mTypeDefComboBoxModel.addElement("Int");
        mTypeDefComboBoxModel.addElement("Bool");
        mTypeDefComboBoxModel.addElement("Float");
        mTypeDefComboBoxModel.addElement("String");
        if (varDefCopy == null) {
            varDefCopy = mVarDef.getCopy();
        }

        // Show the type definitions of the current node modification status.
        for (DataTypeDefinition def : mNode.getTypeDefList()) {
            mTypeDefComboBoxModel.addElement(def.getName());
        }

        // Show the type definitions of all parent nodes of the current node
        SuperNode parentNode = mNode.getParentNode();

        while (parentNode != null) {
            for (DataTypeDefinition def : parentNode.getTypeDefList()) {
                mTypeDefComboBoxModel.addElement(def.getName());
            }

            parentNode = parentNode.getParentNode();
        }

        // Select the type of the variable definition
        mTypeDefComboBox.setSelectedItem(varDefCopy.getType());

        // Select the expression of the variable definition
        if (varDefCopy.getExp() != null) {
            mExpTextField.setText(varDefCopy.getExp().getAbstractSyntax());
        }
        mVarDef = varDefCopy;
    }

    private void selectExp() {
        Expression exp = new CreateExpDialog(null).run();

        if (exp != null) {
            mVarDef.setExp(exp);
            mExpTextField.setText(exp.getAbstractSyntax());
        }
    }

    public VariableDefinition run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mVarDef;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (mNameTextField.getText().isEmpty()) {
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
        } else {
            // Set the variable
            mVarDef.setType((String)mTypeDefComboBoxModel.getSelectedItem());
            mVarDef.setName(mNameTextField.getText().trim());
            dispose(Button.OK);
            /*
            boolean rightType = mVarDef.validate(mNameTextField.getText().trim(),(String) mTypeDefComboBoxModel.getSelectedItem(), isNewVariable);
            if (!rightType) {
                mExpTextField.setBorder(BorderFactory.createLineBorder(Color.red));
                errorMsg.setForeground(Color.red);
                //errorMsg.setText(mVarDef.getmErrorMsg());
            } else {
                dispose(Button.OK);
            }
            */
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }
}
