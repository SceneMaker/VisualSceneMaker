package de.dfki.vsm.editor;

import de.dfki.vsm.editor.dialog.FunDefDialog;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;

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
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
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
   
    private final ArrayList<FunDefDialog> mPanelList;
    private final SceneFlow mSceneFlow;
    private final JPanel mContainer;
    
    public FunctionEditor(SceneFlow sceneflow) {          
        mSceneFlow = sceneflow;
        mContainer = new JPanel();
        mPanelList = new ArrayList<>();
        setMinimumSize(new Dimension(0, 200));
            
        initComponents();
        
        // Add the element editor to the event multicaster
        EventCaster.getInstance().append(this);              
    }
    
     private void initComponents() {        
        mContainer.setLayout(new BoxLayout(mContainer, BoxLayout.Y_AXIS));             
        setViewportView(mContainer);
        createFunctionPanes();
    }
    
    private void createFunctionPanes(){          
        mPanelList.clear();
        mContainer.removeAll();
       
        for (final FunDef funDef: mSceneFlow.getUsrCmdDefMap().values()){   
            final FunDefDialog funDefPanel = new FunDefDialog(funDef);
            mPanelList.add(funDefPanel);  
            mContainer.add(Box.createRigidArea(new Dimension(5, 5)));   
            
            JPanel content = funDefPanel.createPanel();   
        
            content.setMaximumSize(new Dimension(Editor.getInstance().getBounds().width -280,75));
            content.setPreferredSize(new Dimension(Editor.getInstance().getBounds().width-280 ,75));
            content.setMinimumSize(new Dimension(Editor.getInstance().getBounds().width -280,75));
            
            mContainer.add(content);               
            

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
                     funDef.setName(funDefPanel.getNameInput().getText().trim());  
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
        getVerticalScrollBar().setValue(0);
    }
    

    @Override
    public synchronized void update(EventObject event) {         
        
        if (event instanceof FunctionSelectedEvent) {            
            FunDef functionData = ((FunctionSelectedEvent)event).getFunction(); 
            createFunctionPanes();                 
            for (FunDefDialog currentPanel: mPanelList){            
                if(currentPanel.getFunDef().getName().equals(functionData.getName())){
                    System.out.println("selected: "+functionData.getName());
                    currentPanel.setSelectedBackground(true);
                    getVerticalScrollBar().setValue(mPanelList.indexOf(currentPanel)*75);
                }
                else{
                    currentPanel.setSelectedBackground(false);
                }                
            }                     
        } else if (event instanceof NodeSelectedEvent) {
            for (FunDefDialog currentPanel: mPanelList) {                      
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
