package de.dfki.vsm.editor.project.auxiliary.functions;

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionModifiedEvent;
import de.dfki.vsm.editor.event.FunctionRemovedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.model.sceneflow.diagram.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunctionDefinition;
import de.dfki.vsm.model.sceneflow.definition.ArgumentDefinition;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.util.ArrayList;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

/**
* @author Sergio Soto
*/
public class FunctionsEditor extends JPanel implements EventListener {

    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    private JScrollPane mMainScrollPanel;
    private JPanel mFunctionsContainerPanel;

    private ArrayList<SingleFunctionContainer> mFunctionContainerList = new ArrayList<>();;

    private final SceneFlow mSceneFlow;

    public FunctionsEditor(final EditorProject project) {
        mSceneFlow = project.getSceneFlow();

        setLayout(new GridLayout(1, 0));
        initComponents();

        // Add the element editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents() {
        mFunctionsContainerPanel = new JPanel();
        mFunctionsContainerPanel.setOpaque(false);
        mFunctionsContainerPanel.setLayout(new BoxLayout(mFunctionsContainerPanel, BoxLayout.Y_AXIS));
        refreshFunctionsContainerPanel();

        mMainScrollPanel = new JScrollPane(mFunctionsContainerPanel);
        mMainScrollPanel.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        mMainScrollPanel.setOpaque(false);
        mMainScrollPanel.getViewport().setOpaque(false);
        mMainScrollPanel.setMinimumSize(new Dimension(2000, 200));
        mMainScrollPanel.getVerticalScrollBar().setValue(0);
        add(mMainScrollPanel);
    }

    private void refreshFunctionsContainerPanel() {

        // Inititalize panel
        mFunctionContainerList.clear();  
        mFunctionsContainerPanel.removeAll();
        repaint();
        
        // Iterate through all existing functions in the sceneflow
        for (FunctionDefinition i : mSceneFlow.getUsrCmdDefMap().values()) {

            // Create a SingleFunctionContainer object for every existing function
            SingleFunctionContainer singleFunctionContainer 
                    = new SingleFunctionContainer(i, mSceneFlow);

            // Add it to list
            mFunctionContainerList.add(singleFunctionContainer);
      
            mFunctionsContainerPanel.add(Box.createRigidArea(new Dimension(1, 5)));
            mFunctionsContainerPanel.add(singleFunctionContainer);        
        }
        
        mFunctionsContainerPanel.add(Box.createRigidArea(new Dimension(5, 5)));
        
        repaint();
    }
    
    
    public void addNewFunction() {
        FunctionDefinition usrCmdDef = new FunctionDefinition("newCommand", "java.lang.System.out", "println");
        usrCmdDef.addArg(new ArgumentDefinition("text", "String"));
        mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);
        
        SingleFunctionContainer singleFunctionContainer 
                    = new SingleFunctionContainer(usrCmdDef, mSceneFlow);
           
        singleFunctionContainer.updateArgList();    
        
        mEventCaster.convey(new FunctionCreatedEvent(this, usrCmdDef));
    }

    @Override
    public void update(EventObject event) {
        if (event instanceof FunctionSelectedEvent) {
            FunctionDefinition functionData = ((FunctionSelectedEvent) event).getFunction();

            for (SingleFunctionContainer currentPanel : mFunctionContainerList) {
                if (functionData.getName().equals(currentPanel.getFunDef().getName())) {
                    currentPanel.setSelectedBackground(true);
                    mMainScrollPanel.getVerticalScrollBar().setValue(mFunctionContainerList.indexOf(currentPanel) * 75);
                    currentPanel.getNameInput().requestFocusInWindow();
                } else {
                    currentPanel.setSelectedBackground(false);
                }
            }

        } else if (event instanceof FunctionCreatedEvent) {
            refreshFunctionsContainerPanel();

            // Highlight and set scrollbar to selected function
            FunctionDefinition functionData = ((FunctionCreatedEvent) event).getFunction();

            for (SingleFunctionContainer currentPanel : mFunctionContainerList) {
                if (functionData.getName().equals(currentPanel.getFunDef().getName())) {
                    mMainScrollPanel.getVerticalScrollBar().setValue(mFunctionContainerList.indexOf(currentPanel) * 75);
                } else {
                    currentPanel.setSelectedBackground(false);
                }
            }

            // Look for function in list
            for (SingleFunctionContainer currentPanel : mFunctionContainerList) {
                if (functionData.getName().equals(currentPanel.getFunDef().getName())) {
                    currentPanel.getNameInput().requestFocusInWindow();
                    currentPanel.updateFunDef();
                } else {
                    currentPanel.setSelectedBackground(false);
                }
            }

        } else if (event instanceof FunctionModifiedEvent) {
            refreshFunctionsContainerPanel();

            FunctionDefinition functionData = ((FunctionModifiedEvent) event).getFunction();

            // Look for function in list
            for (SingleFunctionContainer currentPanel : mFunctionContainerList) {
                if (functionData.getName().equals(currentPanel.getFunDef().getName())) {
                    currentPanel.getNameInput().requestFocusInWindow();
                    currentPanel.updateFunDef();
                } else {
                    currentPanel.setSelectedBackground(false);
                }
            }
        }
        else if (event instanceof FunctionRemovedEvent) {
            String functionName = ((FunctionRemovedEvent)event).getFunction().getName();
            mSceneFlow.removeUsrCmdDef(functionName);
            refreshFunctionsContainerPanel();
            EditorInstance.getInstance().refresh();
        }
        
    }
   
    public final void refresh() {
        // Refresh the visual appearance
    }


    private class Observable extends java.util.Observable {

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
    
    public final void close() {
        // Remove All Observers
        //mObservable.deleteObservers();
        // TODO: Close the functions editor
    }
}
