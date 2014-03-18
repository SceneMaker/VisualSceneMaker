package de.dfki.vsm.editor;

import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
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
import java.util.Observer;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import java.util.Vector;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.DefaultListModel;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.border.EtchedBorder;
import javax.swing.event.MouseInputAdapter;



/**
 * *****************************************************************************
 *
 * @author Sergio Soto
 *
 *******************************************************************************
 */

public class FunctionEditor extends JScrollPane implements EventListener, Observer {    
   
    private final Observable mObservable = new Observable();    
   
    private final ArrayList<FunPropertiesPanel> mPanelList;
    private SceneFlow mSceneFlow;
    private final JPanel mContainer;
    
    public FunctionEditor(SceneFlow sceneflow) {   
       
        mSceneFlow = sceneflow;
        mContainer = new JPanel();
        mPanelList = new ArrayList<FunPropertiesPanel>();
        setMinimumSize(new Dimension(0, 200));
            
        initComponents();
        
        // Add the element editor to the event multicaster
        EventCaster.getInstance().append(this);              
    }
    
     private void initComponents() {   
        
        mContainer.setLayout(new BoxLayout(mContainer, BoxLayout.Y_AXIS)); 
       
        createFunctionPanes();
        setViewportView(mContainer);
    }
    
    private void createFunctionPanes(){     
       
        mPanelList.clear();
       
        for (FunDef funDef: mSceneFlow.getUsrCmdDefMap().values()){            
              FunPropertiesPanel funDefPanel = new FunPropertiesPanel(funDef);
              funDefPanel.setMaximumSize(new Dimension(10000,75));
              mPanelList.add(funDefPanel);
        }
        
        mContainer.removeAll();
         
        for (FunPropertiesPanel currentPanel: mPanelList){    
             mContainer.add(Box.createRigidArea(new Dimension(5, 5)));
             mContainer.add(currentPanel);
        }    
        
        mContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        getVerticalScrollBar().setValue(0);
    }
     
    @Override
    public void update(EventObject event) {
    
        if (event instanceof FunctionSelectedEvent) {
            FunDef functionData = ((FunctionSelectedEvent)event).getFunction();            
         
            createFunctionPanes();
                    
            for (FunPropertiesPanel currentPanel: mPanelList){            
                if(currentPanel.getFunDef().getName().equals(functionData.getName())){
                    currentPanel.setSelectedBackground(true);
                    getVerticalScrollBar().setValue(mPanelList.indexOf(currentPanel)*75);
                }
                else{
                    currentPanel.setSelectedBackground(false);
                }                
            } 
        } else if (event instanceof NodeSelectedEvent) {
            for (FunPropertiesPanel currentPanel: mPanelList){                      
                currentPanel.setSelectedBackground(false);                           
            }  
            
            getVerticalScrollBar().setValue(0);           
        }
    }

    @Override
    public void update(java.util.Observable obs, Object obj) {
        mObservable.update(obj);
    }
    
    private class Observable extends java.util.Observable {
        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}

/*
 * *****************************************************************************
 *
 * @author Sergio Soto
 *
 *******************************************************************************
 */

class FunPropertiesPanel extends JPanel implements EventListener {
    
    private JLabel mNameLabel;
    private JTextField mNameTextField;
    private JLabel mClassNameLabel;
    private JTextField mClassNameTextField;
    private JLabel mMethodLabel;
    private JComboBox mMethodComboBox;
    private JLabel mArgLabel;
    private JList mArgList;
    
    private  JPanel mUpperPanel; 
    private  JPanel mLowerPanel;
    
    private JPanel nameContainer;
    private JPanel methodContainer;
    private JPanel classNameContainer;
    private JPanel argContainer ;
            
    private Method mSelectedMethod;
    private Vector<String> mArgNameList = new Vector<String>();
    private HashMap<String, String> mNameMap = new HashMap<String, String>();
    private HashMap<String, String> mTypeMap = new HashMap<String, String>();
    private HashMap<String, Method> mMethodMap = new HashMap<String, Method>();
    
    private FunDef mFunDef;
    private JPanel mFunDefContent;
    
    private Color mDefaultColor;
    
    public FunPropertiesPanel(FunDef funDef){
   
        mFunDef = funDef;        
        initComponents();              
        EventCaster.getInstance().append(this);
    }

    private void initComponents() {   
                
       setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));  
        
        mNameLabel = new JLabel("Name:");
        mNameLabel.setOpaque(true);
        mNameTextField = new JTextField();
        mNameTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent evt) {              
                save(mFunDef);
            }
        });
        
        mClassNameLabel = new JLabel("Class:");
        mClassNameLabel.setOpaque(true);
        mClassNameTextField = new JTextField();
        mClassNameTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent evt) {
                classTextFieldKeyTyped(evt);
                save(mFunDef);
            }
        });
        
        mMethodLabel = new JLabel("Method:");
        mMethodLabel.setOpaque(true);
        mMethodComboBox = new JComboBox();
        mMethodComboBox.setModel(new DefaultComboBoxModel());
        mMethodComboBox.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent evt) {
                methodComboBoxActionPerformed(evt);
                save(mFunDef);
            }
        });
        
        mArgLabel = new JLabel("Arguments:");
        mArgLabel.setOpaque(true);
        mArgList = new JList();
        mArgList.setMaximumSize(new Dimension(1000,20));
        mArgList.setBorder(BorderFactory.createEtchedBorder());
        mArgList.setModel(new DefaultListModel());
        mArgList.addMouseListener(new MouseInputAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                argumentListMouseClicked(evt);
                save(mFunDef);
            }
        });
        
        fillComponents();        
        createPanel();
        
        mDefaultColor = getBackground();
    }
    
    private void createPanel() {        
        mFunDefContent = new JPanel();
        mFunDefContent.setLayout(new BoxLayout(mFunDefContent, BoxLayout.Y_AXIS));
        mFunDefContent.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY)); 
        
        nameContainer = new JPanel();
        nameContainer.setLayout(new BoxLayout(nameContainer, BoxLayout.X_AXIS));      
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        nameContainer.add(mNameLabel);
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        nameContainer.add(mNameTextField);
        nameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
                
        methodContainer = new JPanel();
        methodContainer.setLayout(new BoxLayout(methodContainer, BoxLayout.X_AXIS)); 
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        methodContainer.add(mMethodLabel);
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        methodContainer.add(mMethodComboBox);
        methodContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        
        classNameContainer = new JPanel();
        classNameContainer.setLayout(new BoxLayout(classNameContainer, BoxLayout.X_AXIS));     
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        classNameContainer.add(mClassNameLabel);
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        classNameContainer.add(mClassNameTextField);
        classNameContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        
        argContainer = new JPanel();
        argContainer.setLayout(new BoxLayout(argContainer, BoxLayout.X_AXIS)); 
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        argContainer.add(mArgLabel);
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));
         argContainer.add(mArgList);
        argContainer.add(Box.createRigidArea(new Dimension(5, 5)));
  
                    
        mUpperPanel = new JPanel(); 
        mUpperPanel.setOpaque(true);
        mUpperPanel.setLayout(new GridLayout(0,2)); 
        mUpperPanel.add(nameContainer);
        mUpperPanel.add(methodContainer);
         
        mLowerPanel = new JPanel(); 
        mLowerPanel.setOpaque(true);
        mLowerPanel.setLayout(new GridLayout(0,2));
        
        mLowerPanel.add(classNameContainer);
        mLowerPanel.add(argContainer);
                        
        add(Box.createRigidArea(new Dimension(5, 10)));
        add(mUpperPanel);
        add(Box.createRigidArea(new Dimension(5, 3)));
        add(mLowerPanel);
        add(Box.createRigidArea(new Dimension(5, 10)));
        
        setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
    }
    
    public void paintPanel(Color color) {
        setBackground(color);
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
        if(selected){        
            paintPanel(Color.LIGHT_GRAY);                      
            mNameTextField.requestFocus();
        }
        else{
            paintPanel(mDefaultColor);         
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
    

    private void initMethodComboBox() {
        initMethodComboBox(mFunDef.getClassName());
    }

    private void initMethodComboBox(String className) {
        ((DefaultComboBoxModel) mMethodComboBox.getModel()).removeAllElements();
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
    
  
    public void save(FunDef funDef) {
        
        if (mSelectedMethod == null) {
            return;
        }
        
        this.repaint();

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
        
        Editor.getInstance().update();             
    }

    @Override
    public void update(EventObject event) {
         Editor.getInstance().update();  
    }

    public FunDef getFunDef(){   
        return mFunDef;
    }    
}
  
