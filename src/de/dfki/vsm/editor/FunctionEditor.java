package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.command.expression.UsrCmd;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import static java.awt.Component.CENTER_ALIGNMENT;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Observer;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
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
   
    private final ArrayList<FunDefDialog> mFunDefBoxList;
    private final SceneFlow mSceneFlow;
    private final JPanel mContainer;
    private JPanel mButtonPanel;
    private JButton mRemoveButton;
    private JButton mAddFunctionButton;
    
    public FunctionEditor(SceneFlow sceneflow) {          
        mSceneFlow = sceneflow;
        mContainer = new JPanel();
        mFunDefBoxList = new ArrayList<>();
        setMinimumSize(new Dimension(0, 200));            
        initComponents();
        
        // Add the element editor to the event multicaster
        EventCaster.getInstance().append(this);              
    }
    
     private void initComponents() {        
        mContainer.setLayout(new BoxLayout(mContainer, BoxLayout.Y_AXIS));             
        setViewportView(mContainer);
        initButtonPanel();
        createFunctionPanes();
    }
          
    private void initButtonPanel() {   
        
        // Create Button 
        mAddFunctionButton = new JButton("Add Function");
        mAddFunctionButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {        
                addNewFunction();               
            }
        });  
     
        Action action = new AbstractAction("AddFunction") {
            @Override
            public void actionPerformed(ActionEvent e) {
                addNewFunction();
            }
        };
        
        // configure the Action with the accelerator (aka: short cut)
        action.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke("control F"));

        // Button panel
        mButtonPanel = new JPanel(null);        
        mButtonPanel.setLayout(new BoxLayout(mButtonPanel, BoxLayout.Y_AXIS));
        mButtonPanel.setAlignmentX(CENTER_ALIGNMENT);
        mButtonPanel.add(Box.createRigidArea(new Dimension(45, 10)));
        mButtonPanel.add(mAddFunctionButton);    
        mButtonPanel.add(Box.createRigidArea(new Dimension(45, 10)));
       
        // manually register the accelerator in the button's component input map
        mButtonPanel.getActionMap().put("myAction", action);
        mButtonPanel.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(
                (KeyStroke) action.getValue(Action.ACCELERATOR_KEY), "myAction");
        
    }
    
    private void addNewFunction(){
        FunDef usrCmdDef = new FunDef("", "java.lang.System.out", "printIn(String)");
        mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
        Editor.getInstance().update();
        EventCaster.getInstance().convey(new FunctionCreatedEvent(this, usrCmdDef)); 
    }
    
    private void createFunctionPanes(){          
        mFunDefBoxList.clear();
        mContainer.removeAll();
       
        for (final FunDef funDef: mSceneFlow.getUsrCmdDefMap().values()){   
            final FunDefDialog funDefPanel = new FunDefDialog(funDef);
            mFunDefBoxList.add(funDefPanel);  
            mContainer.add(Box.createRigidArea(new Dimension(5, 5)));   
            
            JPanel function = new JPanel();
            function.setLayout(new BoxLayout(function, BoxLayout.X_AXIS));
            JPanel content = funDefPanel.createPanel(); 
            function.add(content);
            
            mRemoveButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/minus.png"));
            mRemoveButton.setMinimumSize(new Dimension(20, 75));
            mRemoveButton.setMaximumSize(new Dimension(20, 75));
            mRemoveButton.setPreferredSize(new Dimension(20, 75));
           
           
            mRemoveButton.setOpaque(true);
            mRemoveButton.setBackground(Color.GRAY);
            mRemoveButton.setBorder(BorderFactory.createEmptyBorder());
            mRemoveButton.addActionListener(new ActionListener() {            
                @Override
                public void actionPerformed(ActionEvent e) {                    
                   for(de.dfki.vsm.model.sceneflow.Node n:mSceneFlow.getNodeList()){
                        Iterator<Command> it = n.getCmdList().iterator();
                        if(it.hasNext()){
                            for (int i = 0; it.hasNext(); i++) {
                                if(((UsrCmd)it.next()).getName().equals(funDef.getName())){
                                    if (JOptionPane.showConfirmDialog(null, "This will remove function usages in workspace \n"
                                                                    + "Are you sure?", "WARNING",
                                                                    JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {       
                                            it.remove();
                                            removeFunction(funDef);
                                    } 
                                    else{
                                        // do nothing
                                    }
                                } 
                            }
                        }
                        else{
                            removeFunction(funDef);                           
                        }                        
                    }
                }
             });
            
            function.add(mRemoveButton);           
            mContainer.add(function);               
            

            // Add focus listeners to editable elements ------------------------ 
           
            funDefPanel.getNameInput().addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent e) {
                   funDefPanel.setSelectedBackground(true);    
                }

                @Override
                public void focusLost(FocusEvent e) {
                    funDefPanel.setSelectedBackground(false); 
                }
            });
           
            funDefPanel.getClassNameInput().addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent e) {
                   funDefPanel.setSelectedBackground(true);  
                }

                @Override
                public void focusLost(FocusEvent e) {
                   funDefPanel.setSelectedBackground(false); 
                }
            });
         
            funDefPanel.getMethodBox().addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent e) {
                   funDefPanel.setSelectedBackground(true);    
                }

                @Override
                public void focusLost(FocusEvent e) {
                     funDefPanel.setSelectedBackground(false); 
                }
            });
            
            funDefPanel.getArgList().addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent e) {
                   funDefPanel.setSelectedBackground(true);    
                }

                @Override
                public void focusLost(FocusEvent e) {
                     funDefPanel.setSelectedBackground(false); 
                }
            });

            // Add action listeners to editable elements -----------------------
            
            funDefPanel.getNameInput().addKeyListener(new KeyAdapter() {
                @Override
                public void keyReleased(KeyEvent evt) {  
                    
                    for(de.dfki.vsm.model.sceneflow.Node n:mSceneFlow.getNodeList()){
                        for(Command c: n.getCmdList()){
                            if(((UsrCmd)c).getName().equals(funDef.getName())){
                                ((UsrCmd)c).setName(funDefPanel.getNameInput().getText().trim());
                            }   
                        }
                    }
                     
                    mSceneFlow.removeUsrCmdDef(funDef.getName());
                    mSceneFlow.putUsrCmdDef(funDefPanel.getNameInput().getText().trim(), funDef);
                    funDef.setName(funDefPanel.getNameInput().getText().trim()); 
                    funDefPanel.getFunDef().setName(funDefPanel.getNameInput().getText().trim());
                    Editor.getInstance().update();           
                }
            });
            
            funDefPanel.getClassNameInput().addKeyListener(new KeyAdapter() {
                @Override
                public void keyReleased(KeyEvent evt) {      
                     funDefPanel.classTextFieldKeyTyped(evt);
                     funDef.setClassName(funDefPanel.getClassNameInput().getText().trim());  
                     Editor.getInstance().update();     
                }
            });
            
            funDefPanel.getMethodBox().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent evt) {                    
                    funDefPanel.methodComboBoxActionPerformed(evt);                     
                    if (funDefPanel.getSelectedMethod() != null){
                        funDef.setMethod(funDefPanel.getSelectedMethod().getName().trim());
                        funDef.getParamList().clear();
                        Enumeration args = ((DefaultListModel) funDefPanel.getArgList().getModel()).elements();
                        while (args.hasMoreElements()) {
                            String argString = (String) args.nextElement();
                            funDef.addParam(
                                new ParamDef(funDefPanel.getNameMap().get(argString), funDefPanel.getTypeMap().get(argString)));
                        }
                        Editor.getInstance().update(); 
                    }
                }
            });
            
            funDefPanel.getArgList().addMouseListener(new MouseInputAdapter() {
                @Override
                public void mouseClicked(MouseEvent evt) {
                    funDefPanel.argumentListMouseClicked(evt);
                    funDef.getParamList().clear();
                    Enumeration args = ((DefaultListModel) funDefPanel.getArgList().getModel()).elements();
                    while (args.hasMoreElements()) {
                        String argString = (String) args.nextElement();
                        funDef.addParam(
                                new ParamDef(funDefPanel.getNameMap().get(argString), funDefPanel.getTypeMap().get(argString)));
                    }
                    Editor.getInstance().update(); 
                }
            });                         
        }
            
        mContainer.add(Box.createRigidArea(new Dimension(5, 5)));
        mContainer.add(mButtonPanel);
        getVerticalScrollBar().setValue(0);
    }    

    @Override
    public void update(EventObject event) {         
        
        if (event instanceof FunctionSelectedEvent) {            
            FunDef functionData = ((FunctionSelectedEvent)event).getFunction();             
            for (FunDefDialog currentPanel: mFunDefBoxList){  
                if(functionData.getName().equals(currentPanel.getFunDef().getName())){
                    currentPanel.setSelectedBackground(true);
                    getVerticalScrollBar().setValue(mFunDefBoxList.indexOf(currentPanel)*75);
                }
                else{
                    currentPanel.setSelectedBackground(false);
                }
            }
        }    
        
        if (event instanceof FunctionCreatedEvent) {            
                 
            createFunctionPanes();     
                   
            // Highlight and set scrollbar to selected function
            FunDef functionData = ((FunctionCreatedEvent)event).getFunction();      
            for (FunDefDialog currentPanel: mFunDefBoxList){                 
                if(functionData.getName().equals(currentPanel.getFunDef().getName())){
                    currentPanel.getNameInput().requestFocus();
                    getVerticalScrollBar().setValue(mFunDefBoxList.indexOf(currentPanel)*75);
                }
                else{
                    currentPanel.setSelectedBackground(false);
                }
            }
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
    
    
    private void removeFunction(FunDef funDef){
        mSceneFlow.removeUsrCmdDef(funDef.getName());
        Editor.getInstance().update();
        EventCaster.getInstance().convey(new FunctionCreatedEvent(this, funDef)); 
    }
}
