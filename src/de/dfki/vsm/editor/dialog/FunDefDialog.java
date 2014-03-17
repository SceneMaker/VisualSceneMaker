package de.dfki.vsm.editor.dialog;

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Vector;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.event.MouseInputAdapter;

/**
 *
 * @author Gregor Mehlmann
 */
public class FunDefDialog extends Dialog {

    // The function definition that has to be maintained
    private final FunDef mFunDef;
    // The Java reflect method that is mapped to
    private Method mSelectedMethod;
    //
    private Vector<String> mArgNameList = new Vector<String>();
    private HashMap<String, String> mNameMap = new HashMap<String, String>();
    private HashMap<String, String> mTypeMap = new HashMap<String, String>();
    private HashMap<String, Method> mMethodMap = new HashMap<String, Method>();
    // GUI Components
    private JLabel mNameLabel;
    private JTextField mNameTextField;
    private JLabel mClassNameLabel;
    private JTextField mClassNameTextField;
    private JLabel mMethodLabel;
    private JComboBox mMethodComboBox;
    private JLabel mArgLabel;
    private JList mArgList;
    private JScrollPane mArgScrollPane;
    private JButton mOkButton;
    private JButton mCancelButton;
    private JLabel mMessageLabel;
    private JPanel mFunDefContent;

    public FunDefDialog(FunDef funDef) {
        super(Editor.getInstance(), "Function Definition", true);
        if (funDef != null) {
            // Modify an existing function definition
            mFunDef = funDef.getCopy();
        } else {
            // Create a new function definition
            mFunDef = new FunDef("newCommand", "java.lang.System.out", "println");
            mFunDef.addParam(new ParamDef("text", "String"));
        }
        initComponents();
        fillComponents();
    }

    private void initComponents() {
        //
        mNameLabel = new JLabel("Name:");
        mNameTextField = new JTextField();
        //
        mClassNameLabel = new JLabel("Class:");
        mClassNameTextField = new JTextField();
        mClassNameTextField.addKeyListener(new KeyAdapter() {

            @Override
            public void keyTyped(KeyEvent evt) {
                classTextFieldKeyTyped(evt);
            }
        });
        //
        mMethodLabel = new JLabel("Method:");
        mMethodComboBox = new JComboBox();
        mMethodComboBox.setModel(new DefaultComboBoxModel());
        mMethodComboBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                methodComboBoxActionPerformed(evt);
            }
        });
        //
        mArgLabel = new JLabel("Arguments:");
        mArgList = new JList();
        mArgList.setModel(new DefaultListModel());
        mArgScrollPane = new JScrollPane(mArgList);
        mArgList.addMouseListener(new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                argumentListMouseClicked(evt);
            }
        });
        //
        mOkButton = new JButton("Ok");
        mOkButton.addActionListener(new ActionListener() {
             @Override
            public void actionPerformed(ActionEvent evt) {
                okActionPerformed();
            }
        });
        //
        mCancelButton = new JButton("Cancel");
        mCancelButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                cancelActionPerformed();
            }
        });
        //
        mMessageLabel = new JLabel();
        //         
        
        addCompoment(mNameLabel, 10, 10, 70, 20);
        addCompoment(mNameTextField, 90, 10, 200, 20);
        addCompoment(mClassNameLabel, 10, 35, 70, 20);
        addCompoment(mClassNameTextField, 90, 35, 200, 20);
        addCompoment(mMethodLabel, 10, 60, 70, 20);
        addCompoment(mMethodComboBox, 90, 60, 200, 20);
        addCompoment(mArgLabel, 10, 85, 70, 20);
        addCompoment(mArgScrollPane, 90, 85, 200, 80);
        addCompoment(mOkButton, 130, 175, 80, 20);
        addCompoment(mCancelButton, 210, 175, 80, 20);
        addCompoment(mMessageLabel, 10, 200, 280, 20);
        packComponents(300, 230);                
    }
       
    private void fillComponents() {
        //
        mNameTextField.setText(mFunDef.getName());
        //
        mClassNameTextField.setText(mFunDef.getClassName());
        // Init the method combo box with the class name of the user command
        // definition and set the selected method to the method of the user
        // command definition.
        initMethodComboBox();
        mMethodComboBox.setSelectedItem(methodToString());
        //mSelectedMethod = mMethodMap.get(mMethodComboBox.getSelectedItem());

        // Resize the argument name list to the size of the parameter
        // list of the selected method and fill the argument name list
        // with the parameter names of the user command definition.
        resizeArgNameList();
        for (int i = 0; (i < mFunDef.getSizeOfParamList())
                && (i < mArgNameList.size()); i++) {
            mArgNameList.set(i, mFunDef.getParamAt(i).getName());
        }
        updateArgList();
    }

    private void initMethodComboBox() {
        initMethodComboBox(mFunDef.getClassName());
    }

    private void initMethodComboBox(String className) {
        ((DefaultComboBoxModel) mMethodComboBox.getModel()).removeAllElements();
        mMessageLabel.setText("Displaying methods of class/object " + className);
        mMessageLabel.setForeground(Color.GREEN);
        boolean isClass = true;
        boolean isObject = true;

        try {
            Class javaClass = Class.forName(className);
            for (Method method : javaClass.getDeclaredMethods()) {
                if (Modifier.isStatic(method.getModifiers()) && Modifier.isPublic(method.getModifiers())) {
                    String methodStr = methodToString(method);
                    ((DefaultComboBoxModel) mMethodComboBox.getModel()).addElement(methodStr);
                    mMethodMap.put(methodStr, method);
                }
            }
        } catch (SecurityException ex) {
            ex.printStackTrace();
        } catch (ClassNotFoundException ex) {
            isClass = false;
        }
        if (!isClass) {
            isObject = true;
            try {
                int dotIndex = className.lastIndexOf('.');
                String parentName = className.substring(0, dotIndex);
                String memberName = className.substring(dotIndex + 1);

                Class parentClass = Class.forName(parentName);
                Field javaField = parentClass.getField(memberName);
                Class javaClass = javaField.getType();

                for (Method method : javaClass.getDeclaredMethods()) {
                    if (!Modifier.isStatic(method.getModifiers())
                            && Modifier.isPublic(method.getModifiers())) {
                        String methodStr = methodToString(method);
                        ((DefaultComboBoxModel) mMethodComboBox.getModel()).addElement(methodStr);
                        mMethodMap.put(methodStr, method);
                    }
                }
            } catch (SecurityException ex) {
                ex.printStackTrace();
            } catch (ClassNotFoundException ex) {
                isObject = false;
            } catch (NoSuchFieldException ex) {
                isObject = false;
            } catch (StringIndexOutOfBoundsException ex) {
                isObject = false;
            }
        }
        // Get the selected method and resize/fill the argument list
        mSelectedMethod = mMethodMap.get((String) mMethodComboBox.getSelectedItem());
        resizeArgNameList();
        updateArgList();
        // Display a message on the message label
        if (!isClass && !isObject) {
            mMessageLabel.setText("Specified class/object not found!");
            mMessageLabel.setForeground(Color.RED);
        }
    }

    private void updateArgList() {
        // Clear the argument name list
        ((DefaultListModel) mArgList.getModel()).clear();
        // Check if there is a method selected
        if (mSelectedMethod != null) {
            // Get the list of parameters for the selected method
            Class[] parameterTypes = mSelectedMethod.getParameterTypes();
            for (int i = 0; i < parameterTypes.length; i++) {
                Class parameterType = parameterTypes[i];
                // Get the name of the parameter and of its type
                String parameterName = mArgNameList.get(i);
                String parameterTypeName = parameterType.getName();//parameterType.getSimpleName();

                String composedParameterName = parameterName + " (" + parameterTypeName + ")";
                // Add the name and the name of the type to the appropriate map
                mNameMap.put(composedParameterName, parameterName);
                mTypeMap.put(composedParameterName, parameterTypeName);
                // Add the argument to the list
                ((DefaultListModel) mArgList.getModel()).addElement(composedParameterName);
            }
        }
    }

    private void resizeArgNameList() {
        if (mSelectedMethod == null) {
            // Resize to zero if there is no method selected
            mArgNameList.setSize(0);
        } else {
            mArgNameList.setSize(mSelectedMethod.getParameterTypes().length);
            for (int i = 0; i < mArgNameList.size(); i++) {
                // If the argument has not yet a name then assign a default name
                if (mArgNameList.get(i) == null) {
                    mArgNameList.set(i, "Arg" + i);
                }
            }
        }
    }

    public void methodComboBoxActionPerformed(ActionEvent evt) {
        // Get the selected method and resize/fill the argument list
        mSelectedMethod = mMethodMap.get((String) mMethodComboBox.getSelectedItem());
        resizeArgNameList();
        updateArgList();
    }

    private void classTextFieldKeyTyped(KeyEvent evt) {
        String className = mClassNameTextField.getText();
        if (!Character.isISOControl(evt.getKeyChar())) {
            int position = mClassNameTextField.getCaret().getDot();
            String newstring = new StringBuffer(className).insert(position, evt.getKeyChar()).toString();
            className = newstring;
        }
        initMethodComboBox(className);
    }

    private void argumentListMouseClicked(MouseEvent evt) {
        if (evt.getButton() == MouseEvent.BUTTON1 && evt.getClickCount() == 2) {
            if (mArgList.getSelectedValue() != null) {
                int index = mArgList.getSelectedIndex();
                String result = (String) JOptionPane.showInputDialog(
                        this,
                        "Rename Parameter:",
                        "Rename Parameter",
                        JOptionPane.PLAIN_MESSAGE,
                        null,
                        null,
                        mNameMap.get((String) mArgList.getSelectedValue()));
                mArgNameList.set(index, result);
                updateArgList();
            }
        }
    }

////////////////////////////////////////////////////////////////////////////////
// Helper Methods
    private String methodToString(Method method) {
        String name = method.getName() + "(";
        for (int i = 0; i < method.getParameterTypes().length; i++) {
            name += method.getParameterTypes()[i].getSimpleName();
            if (i != method.getParameterTypes().length - 1) {
                name += ",";
            }
        }
        return name += ")";
    }

    private String methodToString() {
        String name = mFunDef.getMethod() + "(";
        for (int i = 0; i < mFunDef.getSizeOfParamList(); i++) {
            name += mFunDef.getParamAt(i).getType();
            if (i != mFunDef.getSizeOfParamList() - 1) {
                name += ",";
            }
        }
        return name += ")";
    }

    @Override
    public void okActionPerformed() {
        if (mSelectedMethod == null) {
            dispose(Button.CANCEL);
            return;
        }
    
        // Set the name, class name and method name of the user command definition
        mFunDef.setName(mNameTextField.getText().trim());
        mFunDef.setClassName(mClassNameTextField.getText().trim());
        mFunDef.setMethod(mSelectedMethod.getName().trim());

        // Clear the parameter list and fill it again
        mFunDef.getParamList().clear();
        Enumeration args = ((DefaultListModel) mArgList.getModel()).elements();
        while (args.hasMoreElements()) {
            String argString = (String) args.nextElement();
            mFunDef.addParam(
                    new ParamDef(mNameMap.get(argString), mTypeMap.get(argString)));
        }
        //
        dispose(Button.OK);
    }

    @Override
    protected void cancelActionPerformed() {
        dispose(Button.CANCEL);
    }

    public FunDef run() {
        setVisible(true);
        if (mPressedButton == Button.OK) {
            return mFunDef;
        } else {
            return null;
        }
    }
}
