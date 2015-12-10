package de.dfki.vsm.editor.dialog.definition;

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.dialog.CreateExpDialog;
import de.dfki.vsm.editor.dialog.AbstractDialog;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.diagram.nodes.BasicNode;
import de.dfki.vsm.model.sceneflow.language.command.Expression;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.BoolLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.FloatLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.IntLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.ListRecord;
import de.dfki.vsm.model.sceneflow.language.command.expression.literal.StringLiteral;
import de.dfki.vsm.model.sceneflow.language.command.expression.record.StructRecord;
import de.dfki.vsm.model.sceneflow.language.definition.VariableDefinition;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
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
public final class VarDefDialog extends AbstractDialog {

    //private final Node mNode;
    private VariableDefinition mVariableDefinition;

    // GUI Components
    private JLabel mNameLabel;
    private JLabel mTypeDefLabel;
    private JLabel mExpLabel;
    private HintTextField mNameTextField;
    private HintTextField mExpTextField;
    private JButton mAddExpButton;
    private JComboBox mTypeComboBox;
    private DefaultComboBoxModel mTypeBoxModel;
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private Dimension labelSize = new Dimension(75, 30);
    private Dimension textFielSize = new Dimension(250, 30);
    private JLabel errorMsg;

    public VarDefDialog(final BasicNode node, final VariableDefinition varDef) {
        super(EditorInstance.getInstance(), "Create/Modify Variable Definition", true);
        // Initialize the basic node
        //mNode = node;
        // Initialize the definition
        if (varDef != null) {
            mVariableDefinition = varDef.getCopy();
        }
        //else {
        //    mVariableDefinition = new VariableDefinition("NewVar", "Bool", new BoolLiteral(true));
        //}
        // Initialize the components
        initComponents();
        fillComponents();
    }

    private void initComponents() {

        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new HintTextField("Enter Name");

        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);
        //Name box
        Box nameBox = Box.createHorizontalBox();
        nameBox.add(mNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mNameTextField);

        //
        mTypeDefLabel = new JLabel("Type:");
        mTypeBoxModel = new DefaultComboBoxModel();
        mTypeComboBox = new JComboBox(mTypeBoxModel);

        sanitizeComponent(mTypeDefLabel, labelSize);
        sanitizeComponent(mTypeComboBox, textFielSize);
        //Exp box
        Box typeDefBox = Box.createHorizontalBox();
        typeDefBox.add(mTypeDefLabel);
        typeDefBox.add(Box.createHorizontalStrut(10));
        typeDefBox.add(mTypeComboBox);

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
            @Override
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

        mTypeComboBox.addItemListener(new ItemListener() {

            @Override
            @SuppressWarnings("empty-statement")
            public void itemStateChanged(final ItemEvent event) {

                switch ((String) event.getItem()) {
                    case "Int":
                        mVariableDefinition = new VariableDefinition("NewVar", "Int", new IntLiteral(0));
                        break;
                    case "Bool":
                        mVariableDefinition = new VariableDefinition("NewVar", "Bool", new BoolLiteral(true));
                        break;
                    case "Float":
                        mVariableDefinition = new VariableDefinition("NewVar", "Float", new FloatLiteral(0));
                        break;
                    case "String":
                        mVariableDefinition = new VariableDefinition("NewVar", "String", new StringLiteral(""));
                        break;
                    case "List":
                        mVariableDefinition = new VariableDefinition("NewVar", "List", new ListRecord());
                        break;
                    case "Struct":
                        mVariableDefinition = new VariableDefinition("NewVar", "Struct", new StructRecord());
                        break;
                }
                mExpTextField.setText(mVariableDefinition.getExp().getConcreteSyntax());
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
        errorMsg.setMinimumSize(labelSize);
        //FINAL BOX
        Box finalBox = Box.createVerticalBox();
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(typeDefBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(expBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(mButtonPanel);
        addComponent(finalBox, 10, 20, 400, 290);
        packComponents(420, 300);
        mOkButton.requestFocus();
    }

    
    private void sanitizeComponent(JComponent jb, Dimension dim) {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }

    private void fillComponents() {
        // Show the built-in types
        mTypeBoxModel.addElement("Int");
        mTypeBoxModel.addElement("Bool");
        mTypeBoxModel.addElement("Float");
        mTypeBoxModel.addElement("String");
        mTypeBoxModel.addElement("List");
        mTypeBoxModel.addElement("Struct");

        if (mVariableDefinition != null) {

            mTypeComboBox.setSelectedItem(mVariableDefinition.getType());

            if (!mVariableDefinition.getName().equals("NewVar")) {
                mNameTextField.setText(mVariableDefinition.getName());
            }
            if (mVariableDefinition.getExp() != null) {
                mExpTextField.setText(mVariableDefinition.getExp().getConcreteSyntax());
            }
        }
    }

    private void selectExp() {
        Expression exp = new CreateExpDialog(null).run();

        if (exp != null) {
            mVariableDefinition.setExp(exp);
            mExpTextField.setText(exp.getConcreteSyntax());
        }
    }

    public VariableDefinition run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mVariableDefinition;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if (mNameTextField.getText().length() == 0) {
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
        } else {

            String type = mVariableDefinition.getType();
            boolean rightType = true;

            Expression expression = mVariableDefinition.getExp();
            String textValue = expression.getConcreteSyntax();

            String expFullClass = expression.getClass().getName();
            int lastIndex = expFullClass.lastIndexOf('.');
            String regularClass = expFullClass;
            if (lastIndex >= 0) {
                regularClass = expFullClass.substring(lastIndex + 1);
            }

            //Just made for the regular datatypes
            if (!type.equals(regularClass) && (regularClass.equals("Float") || regularClass.equals("Int")
                    || regularClass.equals("Bool") || regularClass.equals("String") || regularClass.equals("Object"))) {

                rightType = false;
            }
            if (!rightType) {
                mExpTextField.setBorder(BorderFactory.createLineBorder(Color.red));
                errorMsg.setForeground(Color.red);
                errorMsg.setText("Do not match the data type selected");
            } else {
                mVariableDefinition.setName(mNameTextField.getText().trim());
                mVariableDefinition.setType((String) mTypeBoxModel.getSelectedItem());
                dispose(Button.OK);
            }
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }
}
