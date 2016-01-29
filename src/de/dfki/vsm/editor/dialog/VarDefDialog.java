package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Int;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.model.sceneflow.definition.type.ListTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.StructTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import java.awt.Dimension;

//~--- JDK imports ------------------------------------------------------------
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
 * @author Not me
 */
public class VarDefDialog extends Dialog
{

    private final Node mNode;
    private VarDef mVarDef;

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

    public VarDefDialog(Node node, VarDef varDef)
    {
        super(EditorInstance.getInstance(), "Create/Modify Variable Definition", true);
        mNode = node;

        if (varDef != null)
        {
            mVarDef = varDef.getCopy();
        } else
        {
            mVarDef = new VarDef("NewVar", "Bool", new Bool(true));
        }

        initComponents();
        fillComponents();
    }

    private void initComponents()
    {

        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new HintTextField("Enter Name");
        if (!mVarDef.getName().equals("NewVar"))
        {
            mNameTextField.setText(mVarDef.getName());
        }
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);
        //Name box
        Box nameBox = Box.createHorizontalBox();
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
        mAddExpButton.addActionListener(new ActionListener()
        {
            public void actionPerformed(ActionEvent e)
            {
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

        mTypeDefComboBox.addItemListener(new ItemListener()
        {

            @Override
            @SuppressWarnings("empty-statement")
            public void itemStateChanged(ItemEvent e)
            {

                switch ((String) e.getItem())
                {
                    case "Int":
                        mVarDef = new VarDef("NewVar", "Int", new Int(0));
                        break;
                    case "Bool":
                        mVarDef = new VarDef("NewVar", "Bool", new Bool(true));
                        ;
                        break;
                    case "Float":
                        mVarDef = new VarDef("NewVar", "Float", new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Float(0));
                        ;
                        break;
                    case "String":
                        mVarDef = new VarDef("NewVar", "String", new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.String(""));
                        ;
                        break;

                    case "Object":
                        mVarDef = new VarDef("NewVar", "Object", new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Object());
                        ;
                        break;
                    default:
                        String a = "";
                        // Look in the definition list for the data type
                        for (TypeDef def : mNode.getTypeDefList())
                        {
                            if (def.getName().equals((String) e.getItem()))
                            {
                                if (def instanceof ListTypeDef)
                                {
                                    mVarDef = new VarDef("NewVar", "List", new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.List());;
                                } else if (def instanceof StructTypeDef)
                                {
                                    mVarDef = new VarDef("NewVar", "Struct", new de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Struct());;
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
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
                okActionPerformed();
            }
        });
        //
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
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

    /**
     * Set the correct size of the components
     *
     * @param jb
     * @param dim
     */
    private void sanitizeComponent(JComponent jb, Dimension dim)
    {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }

    private void fillComponents()
    {

        // Show the basic built-in types
        mTypeDefComboBoxModel.addElement("Int");
        mTypeDefComboBoxModel.addElement("Bool");
        mTypeDefComboBoxModel.addElement("Float");
        mTypeDefComboBoxModel.addElement("String");
        mTypeDefComboBoxModel.addElement("Object");

        // Show the type definitions of the current node modification status.
        for (TypeDef def : mNode.getTypeDefList())
        {
            mTypeDefComboBoxModel.addElement(def.getName());
        }

        // Show the type definitions of all parent nodes of the current node
        SuperNode parentNode = mNode.getParentNode();

        while (parentNode != null)
        {
            for (TypeDef def : parentNode.getTypeDefList())
            {
                mTypeDefComboBoxModel.addElement(def.getName());
            }

            parentNode = parentNode.getParentNode();
        }

        // Select the type of the variable definition
        mTypeDefComboBox.setSelectedItem(mVarDef.getType());

        // Select the expression of the variable definition
        if (mVarDef.getExp() != null)
        {
            mExpTextField.setText(mVarDef.getExp().getAbstractSyntax());
        }
    }

    private void selectExp()
    {
        Expression exp = new CreateExpDialog(null).run();

        if (exp != null)
        {
            mVarDef.setExp(exp);
            mExpTextField.setText(exp.getAbstractSyntax());
        }
    }

    public VarDef run()
    {
        setVisible(true);

        if (mPressedButton == Button.OK)
        {
            return mVarDef;
        } else
        {
            return null;
        }
    }

    @Override
    protected void okActionPerformed()
    {
        if (mNameTextField.getText().length() == 0)
        {
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
        } else
        {

            String type = mVarDef.getType();
            boolean rightType = true;

            Expression expression = mVarDef.getExp();
            String textValue = expression.getConcreteSyntax();

            String expFullClass = expression.getClass().getName();
            int lastIndex = expFullClass.lastIndexOf('.');
            String regularClass = expFullClass;
            if (lastIndex >= 0)
            {
                regularClass = expFullClass.substring(lastIndex + 1);
            }

            //Just made for the regular datatypes
            if (!type.equals(regularClass) && (regularClass.equals("Float") || regularClass.equals("Int")
                    || regularClass.equals("Bool") || regularClass.equals("String") || regularClass.equals("Object")))
            {

                rightType = false;
            }
            if (!rightType)
            {
                mExpTextField.setBorder(BorderFactory.createLineBorder(Color.red));
                errorMsg.setForeground(Color.red);
                errorMsg.setText("Do not match the data type selected");
            } else
            {
                mVarDef.setName(mNameTextField.getText().trim());
                mVarDef.setType((String) mTypeDefComboBoxModel.getSelectedItem());
                dispose(Button.OK);
            }
        }
    }

    @Override
    protected void cancelActionPerformed()
    {
        dispose(Button.CANCEL);
    }
}
