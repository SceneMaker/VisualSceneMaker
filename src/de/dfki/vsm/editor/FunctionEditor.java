package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
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
import java.util.Observer;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JComponent;
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
    private final EventCaster mEventCaster = EventCaster.getInstance();
    private final ArrayList<FunDefDialog> mFunDefDialogList;
    private final SceneFlow mSceneFlow;
    private final JPanel mFunctionsPanel;
    private JPanel mButtonPanel;
    private JButton mRemoveButton;
    private JButton mAddFunctionButton;
      
    /***************************************************************************
     * 
     **************************************************************************/          
    public FunctionEditor(SceneFlow sceneflow) {          
        mSceneFlow = sceneflow;       
        mFunDefDialogList = new ArrayList<>();
        setMinimumSize(new Dimension(0, 200));  
        
        mFunctionsPanel = new JPanel();
        initComponents();
        
        // Add the element editor to the event multicaster
        EventCaster.getInstance().append(this);              
    }
    
    /***************************************************************************
     * 
     **************************************************************************/ 
    private void initComponents() {        
        mFunctionsPanel.setLayout(new BoxLayout(mFunctionsPanel, BoxLayout.Y_AXIS));             
        setViewportView(mFunctionsPanel);
        initButtonPanel(); 
        createFunctionPanes();               
    }
     
    /***************************************************************************
     * 
     **************************************************************************/ 
    private void createFunctionPanes(){          
        //  Inititalize panel
        mFunDefDialogList.clear();
        mFunctionsPanel.removeAll();
        
        //  Create a FunDefDialog object for every existing function           
        for (final FunDef funDef: mSceneFlow.getUsrCmdDefMap().values()){   
            
            final FunDefDialog funDefPanel = new FunDefDialog(funDef);            
            mFunDefDialogList.add(funDefPanel);  
                                   
            // add content of funDefPanel to the function container
            JPanel functionContent = funDefPanel.createPanel(); 
            JPanel functionContainer = new JPanel();       
            functionContainer.setLayout(new BoxLayout(functionContainer, BoxLayout.X_AXIS));
            functionContainer.add(functionContent);
            
            // add remove button to the far right             
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
                    removeFunction(funDef);
                }
             });            
            functionContainer.add(mRemoveButton);    
            
            mFunctionsPanel.add(Box.createRigidArea(new Dimension(5, 5)));  
            mFunctionsPanel.add(functionContainer);               
            

            /*  
                Add focus listeners to editable elements from the funDefPanel 
                content to highlight the functionContainer being edited
            */
            funDefPanel.getNameInput().addFocusListener(new FocusListener() {
                @Override
                public void focusGained(FocusEvent e) {
                   funDefPanel.setSelectedBackground(true);    
                }

                @Override
                public void focusLost(FocusEvent e) {                    
                    funDefPanel.setSelectedBackground(false); 
                    funDefPanel.getNameInput().setText(funDef.getName());
                    funDefPanel.getNameInput().setForeground(Color.black);
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

            /* 
                Add action listeners to editable elements from the funDefPanel 
            */
            funDefPanel.getNameInput().addKeyListener(new KeyAdapter() {                               
                @Override
                public void keyReleased(KeyEvent evt) { 
                    
                    String newFundDefName = funDefPanel.getNameInput().getText().trim();                                      
                                          
                    if(!(funDef.getName().equals(newFundDefName))){
                        
                        if(!newFundDefName.equals("")){
                            // look if name is already being used by another command
                            if(mSceneFlow.getUsrCmdDef(newFundDefName) != null){
                               // funDefPanel.setErrorBackground();
                                funDefPanel.getNameInput().setForeground(Color.red);
                            }
                            else{
                                //funDefPanel.setSelectedBackground(true);
                                funDefPanel.getNameInput().setForeground(Color.BLACK);                       
                                mSceneFlow.removeUsrCmdDef(funDef.getName());
                                mSceneFlow.putUsrCmdDef(newFundDefName, funDef);
                                funDef.setName(newFundDefName); 
                                funDefPanel.getFunDef().setName(newFundDefName);
                                Editor.getInstance().update();    
                            }
                        }
                    }                      
                }
            });
            
            funDefPanel.getClassNameInput().addKeyListener(new KeyAdapter() {
                @Override
                public void keyReleased(KeyEvent evt) {      
                    funDef.setClassName(funDefPanel.getClassNameInput().getText().trim());                     
                    //funDefPanel.classTextFieldKeyTyped(evt);
                    //Editor.getInstance().update();      
                }
            });
            
            funDefPanel.getMethodBox().addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent evt) { 
                    
                    String newSelectedMethod = null;
                    if(funDefPanel.getMethodBox().getSelectedItem()!=null){
                        newSelectedMethod = funDefPanel.getMethodBox().getSelectedItem().toString();
                    }
                    
                    if (newSelectedMethod != null){
                        funDef.setMethod(newSelectedMethod);   
                        funDefPanel.methodComboBoxActionPerformed(evt);
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
          
        mFunctionsPanel.add(Box.createRigidArea(new Dimension(5, 5)));
        mFunctionsPanel.add(mButtonPanel);
        getVerticalScrollBar().setValue(0);
    }    
     
    /***************************************************************************
     * 
     **************************************************************************/ 
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
    
    /***************************************************************************
     * 
     **************************************************************************/ 
    private void addNewFunction(){         
        FunDef usrCmdDef = new FunDef("", "java.lang.System.out", "printIn(String)");
        mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
        Editor.getInstance().update();
        EventCaster.getInstance().convey(new FunctionCreatedEvent(this, usrCmdDef)); 
    }

    /***************************************************************************
     * 
     **************************************************************************/ 
    @Override
    public void update(EventObject event) {         
        
        if (event instanceof FunctionSelectedEvent) {            
            FunDef functionData = ((FunctionSelectedEvent)event).getFunction();             
            for (FunDefDialog currentPanel: mFunDefDialogList){  
                if(functionData.getName().equals(currentPanel.getFunDef().getName())){
                    currentPanel.setSelectedBackground(true);
                    getVerticalScrollBar().setValue(mFunDefDialogList.indexOf(currentPanel)*75);
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
            for (FunDefDialog currentPanel: mFunDefDialogList){                 
                if(functionData.getName().equals(currentPanel.getFunDef().getName())){
                    currentPanel.getNameInput().requestFocus();
                    getVerticalScrollBar().setValue(mFunDefDialogList.indexOf(currentPanel)*75);
                }
                else{
                    currentPanel.setSelectedBackground(false);
                }
            }
        }   
    }

    /***************************************************************************
     * 
     **************************************************************************/ 
    @Override
    public void update(java.util.Observable obs, Object obj) {
        mObservable.update(obj);
    }
    
    /***************************************************************************
     * 
     **************************************************************************/ 
    private class Observable extends java.util.Observable {
        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
    
    /***************************************************************************
     * 
     **************************************************************************/ 
    private void removeFunction(FunDef funDef){
       
    
     if (funDef != null) {       
            mSceneFlow.removeUsrCmdDef(funDef.getName());
            Editor.getInstance().update();
            launchFunctionCreatedEvent(funDef);              
        }
    }   
    
    /***************************************************************************
     * 
     **************************************************************************/ 
    private void launchFunctionCreatedEvent(FunDef funDef) {                
        FunctionCreatedEvent ev = new FunctionCreatedEvent(this, funDef);
        mEventCaster.convey(ev);                    
    } 
}
