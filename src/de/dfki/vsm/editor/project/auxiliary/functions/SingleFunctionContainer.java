package de.dfki.vsm.editor.project.auxiliary.functions;

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.RemoveButton;
import de.dfki.vsm.editor.event.FunctionRemovedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import de.dfki.vsm.util.evt.EventDispatcher;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
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
import java.util.ArrayList;
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
 * @author Sergio Soto
 *
 * Patrick Dec. 2015: This code has to be redesigned. The current approach
 * works. The next implementation should not follow the concept of constantly
 * rebuilding every SingleFunctionContainer when a new function is added. The
 * next implementation should follow the pattern Model - View - Control. In the
 * current implementation control is mixed with view.
 *
 * Current status: BETA
 */
public class SingleFunctionContainer extends JPanel {

    private final Vector<String> mArgNameList = new Vector<>();
    private final HashMap<String, String> mNameMap = new HashMap<>();
    private final HashMap<String, String> mTypeMap = new HashMap<>();
    private final HashMap<String, Method> mMethodMap = new HashMap<>();
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
    private final FunDef mFunDefBackup;

    private final SceneFlow mSceneFlow;

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
    private JPanel mFunDefContent;
    private JPanel nameContainer;
    private JPanel methodContainer;
    private JPanel classNameContainer;
    private JPanel argContainer;
    private JPanel mUpperPanel;
    private JPanel mLowerPanel;
    private Color mDefaultColor;
    private Boolean mIsValidClass;
    private final Dimension labelSize = new Dimension(100, 30);
    private final Dimension textFielSize = new Dimension(250, 30);

    public SingleFunctionContainer(FunDef funDef, SceneFlow sceneflow) {
        mFunDef = funDef;
        mFunDefBackup = funDef.getCopy();
        mSceneFlow = sceneflow;
        initComponents();
        fillComponents();
        initParams();

        JPanel functionContent = createPanel();
        setOpaque(false);
        setBackground(Color.WHITE);
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createRaisedBevelBorder(),
                BorderFactory.createLoweredBevelBorder()));

        add(Box.createRigidArea(new Dimension(5, 5)));
        add(createRemoveFunctionButton());
        add(Box.createRigidArea(new Dimension(5, 5)));
        add(functionContent);
    }

    private void initParams() {
        try {
            String newSelectedMethodName = getSelectedMethod().getName().trim();

            mFunDef.setMethod(newSelectedMethodName);
            getFunDef().setMethod(newSelectedMethodName);
            mFunDef.getParamList().clear();

            Enumeration args = ((DefaultListModel) getArgList().getModel()).elements();

            while (args.hasMoreElements()) {
                String argString = (String) args.nextElement();

                mFunDef.addParam(new ParamDef(getNameMap().get(argString),
                        getTypeMap().get(argString)));
            }
        } catch (Exception ex) {
            System.err.println("The function created does not exist in the enviroment \n" + ex.getMessage());
        }
        EditorInstance.getInstance().refresh();
    }

    private void initComponents() {
        mNameLabel = new JLabel("Name:");
        mNameTextField = new JTextField();
        mNameTextField.setDocument(mNameDocument);
        sanitizeComponent(mNameLabel, labelSize);
        sanitizeComponent(mNameTextField, textFielSize);

        mClassNameLabel = new JLabel("Class:");
        mClassNameTextField = new JTextField();
        sanitizeComponent(mClassNameLabel, labelSize);
        sanitizeComponent(mClassNameTextField, textFielSize);

//        mClassNameTextField.addKeyListener(new KeyAdapter() {
//            @Override
//            public void keyTyped(KeyEvent evt) {
//                classTextFieldKeyTyped(evt);
//            }
//        });
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

        addListeners();
        return mFunDefContent;
    }

    private RemoveButton createRemoveFunctionButton() {
        // Remove Function Button
        RemoveButton removeFunctionButton;
        removeFunctionButton = new RemoveButton();
        removeFunctionButton.addMouseListener(new java.awt.event.MouseAdapter() {

            @Override
            public synchronized void mouseClicked(java.awt.event.MouseEvent evt) {
                EventDispatcher.getInstance().convey(new FunctionRemovedEvent(this, mFunDef));
            }

            @Override
            public void mouseEntered(MouseEvent me) {
                setBorder(
                        BorderFactory.createCompoundBorder(
                                BorderFactory.createLineBorder(new Color(82, 127, 255), 2),
                                BorderFactory.createLineBorder(new Color(82, 127, 255), 2)));
            }

            @Override
            public void mouseExited(MouseEvent me) {
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(),
                        BorderFactory.createLoweredBevelBorder()));
            }
        });

        return removeFunctionButton;
    }

    /*
     *   Add action and focus listeners to editable elements from
     *   content to highlight the functionContainer being edited
     *   nd save edited changes
     */
    private void addListeners() {
        // Function Name
        getNameInput().addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                setSelectedBackground(true);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2),
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2)));
            }

            @Override
            public void focusLost(FocusEvent e) {
                setSelectedBackground(false);
                getNameInput().setText(mFunDef.getName());
                getNameInput().setForeground(Color.black);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(),
                        BorderFactory.createLoweredBevelBorder()));

            }
        });
        getNameInput().addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent evt) {

                String newFundDefName = getNameInput().getText().trim();

                if (!(mFunDef.getName().equals(newFundDefName))) {
                    if (!newFundDefName.equals("")) {

                        // look if name is already being used by another command
                        if (mSceneFlow.getUsrCmdDef(newFundDefName) != null) {
                            getNameInput().setForeground(Color.red);
                        } else {
                            getNameInput().setForeground(Color.BLACK);
                            mSceneFlow.removeUsrCmdDef(mFunDef.getName());
                            mSceneFlow.putUsrCmdDef(newFundDefName, mFunDef);

                            updateFunDef();
                        }
                    }
                }
            }
        });

        // Function Class Name
        getClassNameInput().addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent evt) {

                //PG: This is bad code - just to fix some issues
                initMethodComboBox(getClassNameInput().getText().trim());
                updateFunDef();

                if (getSelectedMethod() != null) {

                    // updateFunDef(mFunDef, mFunDefDialog);
                    String newSelectedMethodName = getSelectedMethod().getName().trim();

                    mFunDef.setMethod(newSelectedMethodName);
                    getFunDef().setMethod(newSelectedMethodName);
                    mFunDef.getParamList().clear();

                    Enumeration args = ((DefaultListModel) getArgList().getModel()).elements();

                    while (args.hasMoreElements()) {
                        String argString = (String) args.nextElement();

                        mFunDef.addParam(
                                new ParamDef(getNameMap().get(argString), getTypeMap().get(argString)));
                    }

                    EditorInstance.getInstance().refresh();
                }
            }
        });

        getClassNameInput().addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                setSelectedBackground(true);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2),
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2)));
            }

            @Override
            public void focusLost(FocusEvent e) {
                setSelectedBackground(false);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(),
                        BorderFactory.createLoweredBevelBorder()));
            }
        });

        // Function Method
        getMethodBox().addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                setSelectedBackground(true);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2),
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2)));
            }

            @Override
            public void focusLost(FocusEvent e) {
                setSelectedBackground(false);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(),
                        BorderFactory.createLoweredBevelBorder()));
            }
        });

        getMethodBox().addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                if (getIsValidClass()) {

                    if (getMethodBox().getSelectedItem() != null) {
                        setSelectedMethod(
                                getmMethodMap().get((String) getMethodBox().getSelectedItem()));
                    }

                    if (getSelectedMethod() != null) {

                        // updateFunDef(mFunDef, mFunDefDialog);
                        String newSelectedMethodName = getSelectedMethod().getName().trim();

                        mFunDef.setMethod(newSelectedMethodName);
                        getFunDef().setMethod(newSelectedMethodName);
                        methodComboBoxActionPerformed(evt);
                        mFunDef.getParamList().clear();

                        Enumeration args = ((DefaultListModel) getArgList().getModel()).elements();

                        while (args.hasMoreElements()) {
                            String argString = (String) args.nextElement();

                            mFunDef.addParam(
                                    new ParamDef(getNameMap().get(argString), getTypeMap().get(argString)));
                        }

                        EditorInstance.getInstance().refresh();
                    }
                }
            }
        });

        // Function Arguments
        getArgList().addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {
                setSelectedBackground(true);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2),
                        BorderFactory.createLineBorder(new Color(82, 127, 255), 2)));
            }

            @Override
            public void focusLost(FocusEvent e) {
                setSelectedBackground(false);
                setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createRaisedBevelBorder(), BorderFactory.createLoweredBevelBorder()));
            }
        });
        getArgList().addMouseListener(new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                argumentListMouseClicked(evt);
                mFunDef.getParamList().clear();

                Enumeration args = ((DefaultListModel) getArgList().getModel()).elements();

                while (args.hasMoreElements()) {
                    String argString = (String) args.nextElement();

                    mFunDef.addParam(new ParamDef(getNameMap().get(argString),
                            getTypeMap().get(argString)));
                }

                EditorInstance.getInstance().refresh();
            }
        });
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

        for (int i = 0; (i < mFunDef.getSizeOfParamList()) && (i < mArgNameList.size()); i++) {
            mArgNameList.set(i, mFunDef.getParamAt(i).getName());
        }

        updateArgList();
    }

    private void initMethodComboBox(String className) {

        // TODO - CHECK
        Object selectedMethod = ((DefaultComboBoxModel) mMethodComboBox.getModel()).getSelectedItem();
        String selectedMethodStr = null;

        if (selectedMethod != null) {
            selectedMethodStr = (String) selectedMethod;
        }

        ((DefaultComboBoxModel) mMethodComboBox.getModel()).removeAllElements();
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

                    // select the method if it was previously selected
                    if (methodStr.equalsIgnoreCase(selectedMethodStr)) {
                        ((DefaultComboBoxModel) mMethodComboBox.getModel()).setSelectedItem(selectedMethod);
                    }
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
                    getAvailableMethodNames(selectedMethodStr, javaClass);    // PG added 10.1.14: recursively get all available methods
                }
            } catch (ClassNotFoundException | NoSuchFieldException | StringIndexOutOfBoundsException ex) {
                isObject = false;
            }
        }

        // Display a message on the message label
        if (!isClass && !isObject) {
            mIsValidClass = false;
            mMethodComboBox.addItem(selectedMethodStr);
            mMethodComboBox.setToolTipText("Not in Class or Classpath!");
            mMethodComboBox.setForeground(Color.RED.darker());
        } else {
            // Get the selected method and resize/fill the argument list
            mIsValidClass = true;

            mSelectedMethod = mMethodMap.get((String) mMethodComboBox.getSelectedItem());

            mMethodComboBox.setToolTipText("");

            resizeArgNameList();

            // assign the correct argument name
            for (int i = 0; (i < mFunDef.getSizeOfParamList()) && (i < mArgNameList.size()); i++) {
                mArgNameList.set(i, mFunDef.getParamAt(i).getName());
            }

            updateArgList();
        }
    }

    /*
     * Collects recursively all avaiable method names
     */
    private void getAvailableMethodNames(Object selectedMethod, Class c) {
        String selectedMethodStr = null;

        if (selectedMethod != null) {
            selectedMethodStr = (String) selectedMethod;
        }

        for (Method method : c.getDeclaredMethods()) {
            if (Modifier.isPublic(method.getModifiers())) {
                String methodStr = methodToString(method);

                ((DefaultComboBoxModel) mMethodComboBox.getModel()).addElement(methodStr);
                mMethodMap.put(methodStr, method);

                // select the method if it was previously selected
                if (methodStr.equalsIgnoreCase(selectedMethodStr)) {
                    ((DefaultComboBoxModel) mMethodComboBox.getModel()).setSelectedItem(selectedMethod);
                }
            }
        }

        Class sc = c.getSuperclass();

        if (Modifier.isPublic(sc.getModifiers()) && !sc.equals(Object.class)) {
            getAvailableMethodNames(selectedMethod, sc);
        }
    }

    public void updateArgList() {

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

            // Check if the altered method definition may be the same as a 
            // previous one. If so, use the previous argument description 
            String previousSelectedMethodStr = (mFunDefBackup.getMethod() + mFunDefBackup.getParamPrettyPrint()).replaceAll("\\s+", "");
            Method previousSelectedMethod = mMethodMap.get(previousSelectedMethodStr);

            if (previousSelectedMethod != null) {

                if (mSelectedMethod.toString().equalsIgnoreCase(previousSelectedMethod.toString())) {

                    // assign the previous argument name
                    for (int i = 0; (i < mFunDefBackup.getSizeOfParamList()) && (i < mArgNameList.size()); i++) {
                        mArgNameList.set(i, mFunDefBackup.getParamAt(i).getName());
                    }
                }
            }

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

//    public void classTextFieldKeyTyped(KeyEvent evt) {
//        String className = mClassNameTextField.getText();
//
//        //if (!Character.isISOControl(evt.getKeyChar())) {
//            int position = mClassNameTextField.getCaret().getDot();
//            String newstring = new StringBuffer(className).insert(position, evt.getKeyChar()).toString();
//
//            className = newstring;
//
//            initMethodComboBox(className);
//        //} 
//    }
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

    public void updateFunDef() {
        boolean isClass = true;

        mFunDef.setName(getNameInput().getText().trim());
        mFunDef.setClassName(getClassNameInput().getText().trim());

        try {
            Class.forName(mFunDef.getClassName());
        } catch (ClassNotFoundException ex) {
            isClass = false;
        }

        if (isClass) {
            mFunDef.setMethod(getSelectedMethod().getName().trim());

            // Clear the parameter list and fill it again
            mFunDef.getParamList().clear();

            Enumeration args = ((DefaultListModel) getArgList().getModel()).elements();

            while (args.hasMoreElements()) {
                String argString = (String) args.nextElement();

                mFunDef.addParam(new ParamDef(getNameMap().get(argString),
                        getTypeMap().get(argString)));
            }

        }

        EditorInstance.getInstance().refresh();
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

    ////////////////////////////////////////////////////////////////////////////
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
