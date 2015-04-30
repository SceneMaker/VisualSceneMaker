package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.model.sceneflow.definition.MemberDef;
import de.dfki.vsm.model.sceneflow.definition.type.ListTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.StructTypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef;
import de.dfki.vsm.model.sceneflow.definition.type.TypeDef.Flavour;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
import javax.swing.DefaultListModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JTextField;

/**
 * A dialog to create or edit a type definition.
 *
 * @author Gregor Mehlmann
 */
public class TypeDefDialog extends Dialog {

    // The type definition that we want to create or edit.
    private TypeDef mTypeDef;
    private ListTypeDef mListTypeDef;
    private StructTypeDef mStructTypeDef;
    //
    // The list of member definitions if the type definition is a struct.
    //private Vector<MemberDef> mMemberDefListData = new Vector<MemberDef>();
    // GUI Components
    private JLabel mFlavourLabel;
    private JComboBox mFlavourComboBox;
    private JLabel mNameLabel;
    private JTextField mNameTextField;
    private JSeparator mSeperator;
    private JLabel mListTypeLabel;
    private JComboBox mListTypeComboBox;
    private JLabel mMemberDefLabel;
    private JList mMemberDefList;
    private DefaultListModel mMemberDefListModel;
    private JScrollPane mMemberDefScrollPane;
    private AddButton mAddMemberDefButton;
    private RemoveButton mRemoveMemberDefButton;
    private EditButton mEditMemberDefButton;
    private OKButton mOkButton;
    private CancelButton mCancelButton;

    public TypeDefDialog(TypeDef typeDef) {
        super(Editor.getInstance(), "Create/Modify Type Definition", true);
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
                Vector<MemberDef> memberDefList = new Vector<MemberDef>();
                memberDefList.add(new MemberDef("someMember", "Bool"));
                mStructTypeDef = new StructTypeDef("SomeStruct", memberDefList);
                //
                mListTypeDef = (ListTypeDef) mTypeDef;
            }
        } else {
            // Create the default list type def
            mListTypeDef = new ListTypeDef("IntList", "Int");
            // Create the default struct type def
            Vector<MemberDef> memberDefList = new Vector<MemberDef>();
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
        mFlavourLabel = new JLabel("Flavour:");
        mFlavourComboBox = new JComboBox(new Object[]{"List", "Struct"});
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
                } else { /* Error */ }
            }
        });
        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new JTextField();
        //
        mSeperator = new JSeparator(JSeparator.HORIZONTAL);
        //
        mListTypeLabel = new JLabel("Type:");
        mListTypeComboBox = new JComboBox(new Object[]{"Bool", "Int", "Float", "String", "Object"});
        //
        mMemberDefLabel = new JLabel("Members:");
        mMemberDefListModel = new DefaultListModel();
        mMemberDefList = new JList(mMemberDefListModel);
        mMemberDefScrollPane = new JScrollPane(mMemberDefList);
        mAddMemberDefButton = new AddButton();
        mAddMemberDefButton.addMouseListener(new java.awt.event.MouseAdapter() {

            public void mouseClicked(java.awt.event.MouseEvent evt) {
                addMemberDef();
            }
        });
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
        addCompoment(mFlavourLabel, 10, 10, 70, 30);
        addCompoment(mFlavourComboBox, 90, 10, 260, 30);
        //
        addCompoment(mNameLabel, 10, 55, 70, 30);
        addCompoment(mNameTextField, 90, 55, 260, 30);
        //
        addCompoment(mSeperator, 5, 100, 290, 10);
        //
        addCompoment(mListTypeLabel, 10, 120, 70, 30);
        addCompoment(mListTypeComboBox, 90, 120, 260, 30);
        addCompoment(mMemberDefLabel, 10, 120, 70, 30);
        addCompoment(mMemberDefScrollPane, 90, 120, 260, 110);
        addCompoment(mAddMemberDefButton, 355, 140, 20, 20);
        addCompoment(mRemoveMemberDefButton, 355, 170, 20, 20);
        addCompoment(mEditMemberDefButton, 355, 200, 20, 20);
        //
        addCompoment(mCancelButton, 75, 250, 125, 30);
        addCompoment(mOkButton, 225, 250, 125, 30);
        
        packComponents(400, 300);
        setListTypeComponentsVisible(false);
        setStructTypeComponentsVisible(true);
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
            //mMemberDefListData.removeElementAt(index);
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

//    private boolean isMemberDefValid(MemberDef memberDef, int index) {
//        if (memberDef.getName().equals("")) {
//            return false;
//        }
//        for (int i = 0; i < mMemberDefListData.size(); i++) {
//            if (i != index && mMemberDefListData.get(i).getName().equals(memberDef.getName())) {
//                return false;
//            }
//        }
//        return true;
//    }
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

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }
}
