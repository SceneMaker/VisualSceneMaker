package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import de.dfki.vsm.model.sceneflow.definition.type.ListTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.StructTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef.Flavour;
import java.awt.Color;
import java.awt.Dimension;

//~--- JDK imports ------------------------------------------------------------

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;

import javax.swing.DefaultListModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;

/**
 * A dialog to create or edit a type definition.
 *
 * @author Gregor Mehlmann
 */
public class TypeDefDialog extends Dialog {

    // The type definition that we want to create or edit.
    private TypeDef       mTypeDef;
    private ListTypeDef   mListTypeDef;
    private StructTypeDef mStructTypeDef;

    //
    // The list of member definitions if the type definition is a struct.
    // private ArrayList<MemberDef> mMemberDefListData = new ArrayList<MemberDef>();
    // GUI Components
    private JLabel           mFlavourLabel;
    private JComboBox        mFlavourComboBox;
    private JLabel           mNameLabel;
    private HintTextField       mNameTextField;
    private JSeparator       mSeperator;
    private JLabel           mListTypeLabel;
    private JComboBox        mListTypeComboBox;
    private JLabel           mMemberDefLabel;
    private JList            mMemberDefList;
    private DefaultListModel mMemberDefListModel;
    private JScrollPane      mMemberDefScrollPane;
    private AddButton        mAddMemberDefButton;
    private RemoveButton     mRemoveMemberDefButton;
    private EditButton       mEditMemberDefButton;
    private OKButton         mOkButton;
    private CancelButton     mCancelButton;
    private JLabel errorMsg;
    private Dimension            labelSize = new Dimension(75, 30);
    private Dimension            textFielSize = new Dimension(250, 30);
    
    public TypeDefDialog(TypeDef typeDef) {
        super(EditorInstance.getInstance(), "Create/Modify Type Definition", true);

        if (typeDef != null) {

            // Get a copy of the given type def
            mTypeDef = typeDef.getCopy();

            //
            if (mTypeDef instanceof StructTypeDef) {

                // Create the default list type def
                mListTypeDef = new ListTypeDef("IntList", "Int");

                //
                mStructTypeDef = (StructTypeDef) mTypeDef;
            } else {

                // Create the default struct type def
                ArrayList<MemberDef> memberDefList = new ArrayList<MemberDef>();

                memberDefList.add(new MemberDef("someMember", "Bool"));
                mStructTypeDef = new StructTypeDef("SomeStruct", memberDefList);

                //
                mListTypeDef = (ListTypeDef) mTypeDef;
            }
        } else {

            // Create the default list type def
            mListTypeDef = new ListTypeDef("IntList", "Int");

            // Create the default struct type def
            ArrayList<MemberDef> memberDefList = new ArrayList<MemberDef>();

            memberDefList.add(new MemberDef("someMember", "Bool"));
            mStructTypeDef = new StructTypeDef("SomeStruct", memberDefList);

            // Set the default type def to the struct type def
            mTypeDef = mStructTypeDef;
        }

        initComponents();
        fillComponents();
    }

    private void initComponents() {

        //
        mFlavourLabel    = new JLabel("Flavour:");
        mFlavourComboBox = new JComboBox(new Object[] { "List", "Struct" });
        mFlavourComboBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String selectedFlavour = (String) mFlavourComboBox.getSelectedItem();

                if (selectedFlavour.equals("List")) {
                    setListTypeComponentsVisible(true);
                    setStructTypeComponentsVisible(false);

                    //
                    mTypeDef = mListTypeDef;
                } else if (selectedFlavour.equals("Struct")) {
                    setListTypeComponentsVisible(false);
                    setStructTypeComponentsVisible(true);

                    //
                    mTypeDef = mStructTypeDef;
                } else { /* Error */
                }
            }
        });
        sanitizeComponent(mFlavourLabel, labelSize);
        sanitizeComponent(mFlavourComboBox, textFielSize);
        //flavour box
        Box flavourBox = Box.createHorizontalBox();
        flavourBox.add(mFlavourLabel);
        flavourBox.add(Box.createHorizontalStrut(10));
        flavourBox.add(mFlavourComboBox);
        //
        mNameLabel     = new JLabel("Name:");
        mNameTextField = new HintTextField("Enter Name");
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);
        //Name box
        Box nameBox = Box.createHorizontalBox();
        nameBox.add(mNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mNameTextField);
        //
        mSeperator = new JSeparator(JSeparator.HORIZONTAL);

        //
        mListTypeLabel    = new JLabel("Type:");
        mListTypeComboBox = new JComboBox(new Object[] { "Bool", "Int", "Float", "String", "Object" });
        sanitizeComponent(mListTypeLabel, labelSize);
        sanitizeComponent(mListTypeComboBox, textFielSize);
        //type box
        Box typeBox = Box.createHorizontalBox();
        typeBox.add(mListTypeLabel);
        typeBox.add(Box.createHorizontalStrut(10));
        typeBox.add(mListTypeComboBox);
        //
        mMemberDefLabel      = new JLabel("Members:");
        mMemberDefListModel  = new DefaultListModel();
        mMemberDefList       = new JList(mMemberDefListModel);
        mMemberDefScrollPane = new JScrollPane(mMemberDefList);
        mAddMemberDefButton  = new AddButton();
        mAddMemberDefButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addMemberDef();
            }
        });
        sanitizeComponent(mMemberDefLabel, labelSize);
        sanitizeComponent(mMemberDefScrollPane, new Dimension(220, 110));
        //
        mRemoveMemberDefButton = new RemoveButton();
        mRemoveMemberDefButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                removeMemberDef();
            }
        });

        //
        mEditMemberDefButton = new EditButton();
        mEditMemberDefButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                editMemberDef();
            }
        });
        // Init alternative start node panel
        Box buttonsBox = Box.createVerticalBox();
        buttonsBox.setMaximumSize(new Dimension(20, 100));
        buttonsBox.add(mAddMemberDefButton);
        buttonsBox.add(Box.createVerticalStrut(10));
        buttonsBox.add(mRemoveMemberDefButton);
        buttonsBox.add(Box.createVerticalStrut(10));
        buttonsBox.add(mEditMemberDefButton);
        JPanel mMemberDefPanel = new JPanel();
        mMemberDefPanel.setLayout(new BoxLayout(mMemberDefPanel, BoxLayout.X_AXIS));
        mMemberDefPanel.add(mMemberDefLabel);
        mMemberDefPanel.add(Box.createHorizontalStrut(10));
        mMemberDefPanel.add(mMemberDefScrollPane);
        mMemberDefPanel.add(Box.createHorizontalStrut(10));
        mMemberDefPanel.add(buttonsBox);
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
        
        //Error message
        errorMsg = new JLabel("Information Required");
        errorMsg.setForeground(Color.white);
        errorMsg.setMinimumSize(labelSize);
        
        // Button panel
        JPanel mButtonPanel = new JPanel();
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.X_AXIS));
        mButtonPanel.add(Box.createHorizontalGlue());
        mButtonPanel.add(mCancelButton);
        mButtonPanel.add(Box.createHorizontalStrut(30));
        mButtonPanel.add(mOkButton);
        mButtonPanel.add(Box.createHorizontalStrut(10));
        
        // Init main panel
        Box finalBox = Box.createVerticalBox();
        finalBox.setAlignmentX(CENTER_ALIGNMENT);
        finalBox.add(flavourBox);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mMemberDefPanel);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(errorMsg);
        finalBox.add(Box.createVerticalStrut(20));
        finalBox.add(mButtonPanel);

        addComponent(finalBox, 10, 30, 380, 320);
        
        packComponents(400, 350);
        setListTypeComponentsVisible(false);
        setStructTypeComponentsVisible(true);
        mOkButton.requestFocus();
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
    private void fillComponents() {
        mFlavourComboBox.setSelectedItem(mTypeDef.getFlavour().name());
        mNameTextField.setText(mTypeDef.getName());

        if (mTypeDef.getFlavour() == Flavour.List) {
            setListTypeComponentsVisible(true);
            setStructTypeComponentsVisible(false);
            mListTypeComboBox.setSelectedItem(((ListTypeDef) mTypeDef).getType());
        } else {
            setListTypeComponentsVisible(false);
            setStructTypeComponentsVisible(true);

            for (MemberDef member : ((StructTypeDef) mTypeDef).getMemberDefList()) {
                mMemberDefListModel.addElement(member.getConcreteSyntax());
            }
        }
    }

    private void setListTypeComponentsVisible(boolean flag) {
        mListTypeLabel.setVisible(flag);
        mListTypeComboBox.setVisible(flag);
    }

    private void setStructTypeComponentsVisible(boolean flag) {
        mMemberDefLabel.setVisible(flag);
        mMemberDefList.setVisible(flag);
        mMemberDefScrollPane.setVisible(flag);
        mAddMemberDefButton.setVisible(flag);
        mRemoveMemberDefButton.setVisible(flag);
        mEditMemberDefButton.setVisible(flag);
    }

    private void addMemberDef() {
        MemberDef memberDef = new MemberDefDialog(this, null).run();

        if (memberDef != null) {
            mMemberDefListModel.addElement(memberDef.getConcreteSyntax());
            ((StructTypeDef) mTypeDef).addMemberDef(memberDef);

            // mMemberDefListData.add(memberDef);
        }
    }

    private void removeMemberDef() {
        int index = mMemberDefList.getSelectedIndex();

        if (index >= 0) {
            mMemberDefListModel.removeElementAt(index);
            ((StructTypeDef) mTypeDef).removeMemberDefAt(index);

            // mMemberDefListData.removeElementAt(index);
        }
    }

    private void editMemberDef() {
        int index = mMemberDefList.getSelectedIndex();

        if (index >= 0) {
            MemberDef oldMemberDef = ((StructTypeDef) mTypeDef).getMemberDefAt(index);
            MemberDef newMemberDef = new MemberDefDialog(this, oldMemberDef).run();

            if (newMemberDef != null) {
                mMemberDefListModel.set(index, newMemberDef.getConcreteSyntax());
                ((StructTypeDef) mTypeDef).setMemberDefAt(index, newMemberDef);
            }
        }
    }

//  private boolean isMemberDefValid(MemberDef memberDef, int index) {
//      if (memberDef.getName().equals("")) {
//          return false;
//      }
//      for (int i = 0; i < mMemberDefListData.size(); i++) {
//          if (i != index && mMemberDefListData.get(i).getName().equals(memberDef.getName())) {
//              return false;
//          }
//      }
//      return true;
//  }
    public TypeDef run() {
        setVisible(true);

        if (mPressedButton == Button.OK) {
            return mTypeDef;
        } else {
            return null;
        }
    }

    @Override
    protected void okActionPerformed() {
        if(mNameTextField.getText().length() == 0){
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
        }
        else
        {
            // Get the flavour
            Flavour flavour = Flavour.valueOf((String) mFlavourComboBox.getSelectedItem());
            //
            if (flavour == Flavour.List) {
                ((ListTypeDef) mTypeDef).setName(mNameTextField.getText().trim());
                ((ListTypeDef) mTypeDef).setType((String) mListTypeComboBox.getSelectedItem());
            } else {
                ((StructTypeDef) mTypeDef).setName(mNameTextField.getText().trim());
            }
            //
            dispose(Button.OK);
        }
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }
}
