package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------
import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
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

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.event.MouseInputAdapter;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;

/**
 *
 * @author Gregor Mehlmann
 */
public class FunDefDialog extends Dialog {

    //
    private Vector<String> mArgNameList = new Vector<String>();
    private HashMap<String, String> mNameMap = new HashMap<String, String>();
    private HashMap<String, String> mTypeMap = new HashMap<String, String>();
    private HashMap<String, Method> mMethodMap = new HashMap<String, Method>();
    private final Document mNameDocument = new PlainDocument() {
        @Override
        public void insertString(int offs, String str, AttributeSet attr) throws BadLocationException {
            String newstr = str.replaceAll(" ", "");    // could use "\\s" instead of " "

            super.insertString(offs, newstr, attr);
        }

        @Override
        public void replace(int offs, int len, String str, AttributeSet attr) throws BadLocationException {
            String newstr = str.replaceAll(" ", "");    // could use "\\s" instead of " "

            super.replace(offs, len, newstr, attr);
        }
    };

    // The function definition that has to be maintained
    private final FunDef mFunDef;

    // The Java reflect method that is mapped to
    private Method mSelectedMethod;

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
    private OKButton mOkButton;
    private CancelButton mCancelButton;
    private JLabel mMessageLabel;
    private JPanel mFunDefContent;
    private JPanel nameContainer;
    private JPanel methodContainer;
    private JPanel classNameContainer;
    private JPanel argContainer;
    private JPanel mUpperPanel;
    private JPanel mLowerPanel;
    private Color mDefaultColor;
    private Boolean mIsValidClass;
    private Dimension labelSize = new Dimension(100, 30);
    private Dimension textFielSize = new Dimension(250, 30);

    public FunDefDialog(FunDef funDef) {
        super(EditorInstance.getInstance(), "Function Definition", true);

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
        mNameLabel = new JLabel("Name:");
        mNameTextField = new JTextField();
        mNameTextField.setDocument(mNameDocument);
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);

        //Name box
        Box nameBox = Box.createHorizontalBox();
        nameBox.add(mNameLabel);
        nameBox.add(Box.createHorizontalStrut(10));
        nameBox.add(mNameTextField);
        //
        mClassNameLabel = new JLabel("Class:");
        mClassNameTextField = new JTextField();
        sanitizeComponent(mClassNameLabel, labelSize);
        sanitizeComponent(mClassNameTextField, textFielSize);

        mClassNameTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyTyped(KeyEvent evt) {
                if (!evt.isActionKey()) {
                    classTextFieldKeyTyped(evt);
                }
            }
        });
        //Class name box
        Box classNameBox = Box.createHorizontalBox();
        classNameBox.add(mClassNameLabel);
        classNameBox.add(Box.createHorizontalStrut(10));
        classNameBox.add(mClassNameTextField);
        //
        mMethodLabel = new JLabel("Method:");
        mMethodComboBox = new JComboBox();
        sanitizeComponent(mMethodLabel, labelSize);
        sanitizeComponent(mMethodComboBox, textFielSize);
        mMethodComboBox.setModel(new DefaultComboBoxModel());
        mMethodComboBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                methodComboBoxActionPerformed(evt);
            }
        });
        //Method box
        Box methodBox = Box.createHorizontalBox();
        methodBox.add(mMethodLabel);
        methodBox.add(Box.createHorizontalStrut(10));
        methodBox.add(mMethodComboBox);
        //
        mArgLabel = new JLabel("Arguments:");
        mArgList = new JList();
        mArgList.setModel(new DefaultListModel());
        mArgScrollPane = new JScrollPane(mArgList);
        mArgScrollPane.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        sanitizeComponent(mArgLabel, labelSize);
        sanitizeComponent(mArgScrollPane, new Dimension(220, 110));

        mArgList.addMouseListener(new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                argumentListMouseClicked(evt);
            }
        });
        //Argument box
        Box argBox = Box.createHorizontalBox();
        argBox.add(mArgLabel);
        argBox.add(Box.createHorizontalStrut(10));
        argBox.add(mArgScrollPane);
        // ok button
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
            }
        });
        // cancel button
        mCancelButton = new CancelButton();
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
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
        //
        mMessageLabel = new JLabel();
        Box messageBox = Box.createHorizontalBox();
        messageBox.add(mMessageLabel);
//        mMessageLabel.setMinimumSize(new DimensioXn(300, 30));

        Box finalBox = Box.createVerticalBox();
        finalBox.add(nameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(classNameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(methodBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(classNameBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(messageBox);
        finalBox.add(Box.createVerticalStrut(15));
        finalBox.add(mButtonPanel);

        addComponent(finalBox, 20, 20, 380, 300);
        packComponents(420, 320);
        mDefaultColor = Color.white;
    }

    /**
     * Set the correct size of the components
     *
     * @param jb
     * @param dim
     */
    private void sanitizeComponent(JComponent jb, Dimension dim) {
        jb.setPreferredSize(dim);
        jb.setMinimumSize(dim);
        jb.setMaximumSize(dim);
    }

    private void fillComponents() {
        mNameTextField.setText(mFunDef.getName());

        mClassNameTextField.setText(mFunDef.getClassName());

        // Init the method combo box with the class name of the user command
        // definition and set the selected method to the method of the user
        // command definition.
        initMethodComboBox(mFunDef.getClassName());

        String selectedMethod = mFunDef.getMethod() + mFunDef.getParamPrettyPrint();

        selectedMethod = selectedMethod.replaceAll("\\s+", "");
        mMethodComboBox.setSelectedItem(selectedMethod);
        mSelectedMethod = mMethodMap.get(selectedMethod);

        // Resize the argument name list to the size of the parameter
        // list of the selected method and fill the argument name list
        // with the parameter names of the user command definition.
        resizeArgNameList();

        // TODO - Something is wrong with the argument name!
        
        for (int i = 0; (i < mFunDef.getSizeOfParamList()) && (i < mArgNameList.size()); i++) {
            mArgNameList.set(i, mFunDef.getParamAt(i).getName());
        }

        updateArgList();
    }

    private void initMethodComboBox(String className) {
        ((DefaultComboBoxModel) mMethodComboBox.getModel()).removeAllElements();
        mMessageLabel.setText("Displaying methods of class/object " + className);
        mMessageLabel.setForeground(Color.GREEN);
        mMethodComboBox.setForeground(Color.DARK_GRAY);

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

                if (Modifier.isStatic(javaField.getModifiers()) && Modifier.isPublic(javaField.getModifiers())) {
                    getAvailableMethodNames(javaClass);    // PG added 10.1.14: recursively get all available methods
                }
            } catch (ClassNotFoundException | NoSuchFieldException | StringIndexOutOfBoundsException ex) {
                isObject = false;
            }
        }

        // Display a message on the message label
        if (!isClass && !isObject) {
            mIsValidClass = false;
            mMethodComboBox.addItem(mFunDef.getMethod() + " (not in class or classpath)");
            mMethodComboBox.setForeground(Color.RED.darker());
            mMessageLabel.setText("Specified class/object not found!");
            mMessageLabel.setForeground(Color.RED.darker());
        } else {

            // Get the selected method and resize/fill the argument list
            mIsValidClass = true;
            mSelectedMethod = mMethodMap.get((String) mMethodComboBox.getSelectedItem());
            resizeArgNameList();
            updateArgList();
        }
    }

    /*
     * Collects recursively all avaiable method names
     */
    private void getAvailableMethodNames(Class c) {
        for (Method method : c.getDeclaredMethods()) {
            if (Modifier.isPublic(method.getModifiers())) {
                String methodStr = methodToString(method);

                ((DefaultComboBoxModel) mMethodComboBox.getModel()).addElement(methodStr);
                mMethodMap.put(methodStr, method);
            }
        }

        Class sc = c.getSuperclass();

        if (Modifier.isPublic(sc.getModifiers()) && !sc.equals(Object.class)) {
            getAvailableMethodNames(sc);
        }
    }

    public JPanel createPanel() {
        mFunDefContent = new JPanel();
        mFunDefContent.setLayout(new BoxLayout(mFunDefContent, BoxLayout.Y_AXIS));

        // mFunDefContent.setLayout(new GridLayout(0,1));
        mFunDefContent.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY));

        nameContainer = new JPanel();
        nameContainer.setLayout(new BoxLayout(nameContainer, BoxLayout.X_AXIS));
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        nameContainer.add(mNameLabel);
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        nameContainer.add(mNameTextField);

        mNameTextField.setPreferredSize(new Dimension(0, 25));
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));

        methodContainer = new JPanel();
        methodContainer.setLayout(new BoxLayout(methodContainer, BoxLayout.X_AXIS));
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        methodContainer.add(mMethodLabel);
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        methodContainer.add(mMethodComboBox);

        mMethodComboBox.setPreferredSize(new Dimension(0, 25));
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));

        classNameContainer = new JPanel();
        classNameContainer.setLayout(new BoxLayout(classNameContainer, BoxLayout.X_AXIS));
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        classNameContainer.add(mClassNameLabel);
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        classNameContainer.add(mClassNameTextField);

        mClassNameTextField.setPreferredSize(new Dimension(0, 25));
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));

        argContainer = new JPanel();

        JList list = mArgList;
        list.setVisibleRowCount(1);
        list.setLayoutOrientation(JList.HORIZONTAL_WRAP);

        argContainer.setLayout(new BoxLayout(argContainer, BoxLayout.X_AXIS));
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        argContainer.add(mArgLabel);
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        argContainer.add(list);

        list.setPreferredSize(new Dimension(0, 25));
        DefaultListCellRenderer renderer = (DefaultListCellRenderer) list.getCellRenderer();
        renderer.setHorizontalAlignment(JLabel.CENTER);
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));

        mUpperPanel = new JPanel();
        mUpperPanel.setOpaque(true);
        mUpperPanel.setLayout(new GridLayout(0, 2));
        mUpperPanel.add(nameContainer);
        mUpperPanel.add(methodContainer);

        mLowerPanel = new JPanel();
        mLowerPanel.setOpaque(true);
        mLowerPanel.setLayout(new GridLayout(0, 2));
        mLowerPanel.add(classNameContainer);
        mLowerPanel.add(argContainer);

        mFunDefContent.add(Box.createRigidArea(new Dimension(5, 10)));
        mFunDefContent.add(mUpperPanel);
        mFunDefContent.add(Box.createRigidArea(new Dimension(5, 5)));
        mFunDefContent.add(mLowerPanel);
        mFunDefContent.add(Box.createRigidArea(new Dimension(5, 10)));

        Dimension screenSize = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        double width = screenSize.getWidth();

        mFunDefContent.setMaximumSize(new Dimension((int) width, 80));

        return mFunDefContent;
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

                // String parameterTypeName = parameterType.getName();//parameterType.getSimpleName();
                String parameterTypeName = parameterType.getName();
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

    public void classTextFieldKeyTyped(KeyEvent evt) {
        String className = mClassNameTextField.getText();

        if (!Character.isISOControl(evt.getKeyChar())) {
            int position = mClassNameTextField.getCaret().getDot();
            String newstring = new StringBuffer(className).insert(position, evt.getKeyChar()).toString();

            className = newstring;
        }

        initMethodComboBox(className);
    }

    public void argumentListMouseClicked(MouseEvent evt) {
        if ((evt.getButton() == MouseEvent.BUTTON1) && (evt.getClickCount() == 2)) {
            if (mArgList.getSelectedValue() != null) {
                int index = mArgList.getSelectedIndex();
                String result = (String) JOptionPane.showInputDialog(this, "Rename Parameter:", "Rename Parameter",
                        JOptionPane.PLAIN_MESSAGE, null, null,
                        mNameMap.get((String) mArgList.getSelectedValue()));

                mArgNameList.set(index, result);
                updateArgList();
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
//  Helper Methods
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
        return mFunDef.getMethod();

        /*
         * String name = mFunDef.getMethod() + "(";
         * for (int i = 0; i < mFunDef.getSizeOfParamList(); i++) {
         * name += mFunDef.getParamAt(i).getType();
         * if (i != mFunDef.getSizeOfParamList() - 1) {
         * name += ",";
         * }
         * }
         * return name += ")";
         */
    }

    @Override
    public void okActionPerformed() {
        if (mNameTextField.getText().length() == 0) {
            mNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            mMessageLabel.setText("Function name is not valid");
            mMessageLabel.setForeground(Color.red);
            return;
        }
        if (mClassNameTextField.getText().length() == 0) {
            mClassNameTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            mMessageLabel.setText("Class name is not valid");
            mMessageLabel.setForeground(Color.red);
            return;
        }
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

            mFunDef.addParam(new ParamDef(mNameMap.get(argString), mTypeMap.get(argString)));
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

    public FunDef getFunDef() {
        return mFunDef;
    }

    public JTextField getNameInput() {
        return mNameTextField;
    }

    public JTextField getClassNameInput() {
        return mClassNameTextField;
    }

    public JComboBox getMethodBox() {
        return mMethodComboBox;
    }

    public Method getSelectedMethod() {
        return mSelectedMethod;
    }

    public JList getArgList() {
        return mArgList;
    }

    public JPanel getContent() {
        return mFunDefContent;
    }

    public HashMap<String, Method> getmMethodMap() {
        return mMethodMap;
    }

    public HashMap<String, String> getNameMap() {
        return mNameMap;
    }

    public HashMap<String, String> getTypeMap() {
        return mTypeMap;
    }

    public void paintPanel(Color color) {
        mFunDefContent.setBackground(color);
        mNameLabel.setBackground(color);
        mClassNameLabel.setBackground(color);
        mMethodLabel.setBackground(color);
        mArgLabel.setBackground(color);
        mUpperPanel.setBackground(color);
        mLowerPanel.setBackground(color);
        nameContainer.setBackground(color);
        methodContainer.setBackground(color);
        classNameContainer.setBackground(color);
        argContainer.setBackground(color);
    }

    public void setSelectedBackground(boolean selected) {
        if (selected) {
            paintPanel(Color.LIGHT_GRAY);
        } else {
            paintPanel(mDefaultColor);
        }
    }

    public void setErrorBackground() {
        paintPanel(Color.ORANGE);
    }

    public Boolean getIsValidClass() {
        return mIsValidClass;
    }

    public void setSelectedMethod(Method value) {
        mSelectedMethod = value;
    }
}
