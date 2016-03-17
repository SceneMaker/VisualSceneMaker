package de.dfki.vsm.editor.project.auxiliary.functions;

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.action.RedoAction;
import de.dfki.vsm.editor.action.UndoAction;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.event.FunctionCreatedEvent;
import de.dfki.vsm.editor.event.FunctionModifiedEvent;
import de.dfki.vsm.editor.event.FunctionRemovedEvent;
import de.dfki.vsm.editor.event.FunctionSelectedEvent;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.definition.FunDef;
import de.dfki.vsm.model.sceneflow.definition.ParamDef;
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
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;

/**
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
public class FunctionsEditor extends JPanel implements EventListener
{

    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    private JScrollPane mMainScrollPanel;
    private JPanel mFunctionsContainerPanel;
    protected UndoManager mUndoManager = null;

    private ArrayList<SingleFunctionContainer> mFunctionContainerList = new ArrayList<>();
    ;

    private final SceneFlow mSceneFlow;
    private String defaulFuncName = "newCommand";
    private int nameCounter = 0;

    public FunctionsEditor(final EditorProject project)
    {
        mSceneFlow = project.getSceneFlow();

        setLayout(new GridLayout(1, 0));
        initComponents();

        // Add the element editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents()
    {
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

    private void refreshFunctionsContainerPanel()
    {

        // Inititalize panel
        mFunctionContainerList.clear();
        mFunctionsContainerPanel.removeAll();
        repaint();

        // Iterate through all existing functions in the sceneflow
        for (FunDef i : mSceneFlow.getUsrCmdDefMap().values())
        {

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

    private boolean containsFunction(String name)
    {
        for (SingleFunctionContainer currentPanel : mFunctionContainerList)
        {
            if (name.equals(currentPanel.getFunDef().getName()))
            {
                return true;
            }
        }
        return false;
    }

    public void addNewFunction()
    {
        String funcName = defaulFuncName;
        while (containsFunction(funcName))
        {
            nameCounter++;
            funcName = defaulFuncName + nameCounter;
        }
        FunDef usrCmdDef = new FunDef(funcName, "java.lang.System.out", "println");
        usrCmdDef.addParam(new ParamDef("text", "String"));
        mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);

        SingleFunctionContainer singleFunctionContainer
            = new SingleFunctionContainer(usrCmdDef, mSceneFlow);

        singleFunctionContainer.updateArgList();

        mUndoManager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();
        mUndoManager.addEdit(new Edit(usrCmdDef, true));
        UndoAction.getInstance().refreshUndoState();
        RedoAction.getInstance().refreshRedoState();

        mEventCaster.convey(new FunctionCreatedEvent(this, usrCmdDef));
    }

    public void redoAddFunction(FunDef usrCmdDef)
    {
        mSceneFlow.putUsrCmdDef(usrCmdDef.getName(), usrCmdDef);

        SingleFunctionContainer singleFunctionContainer
            = new SingleFunctionContainer(usrCmdDef, mSceneFlow);

        singleFunctionContainer.updateArgList();

        mEventCaster.convey(new FunctionCreatedEvent(this, usrCmdDef));
        refreshFunctionsContainerPanel();
        EditorInstance.getInstance().refresh();
    }

    public void undoDeleteFunction(FunDef usrCmdDef)
    {
        mSceneFlow.removeUsrCmdDef(usrCmdDef.getName());

        refreshFunctionsContainerPanel();
        EditorInstance.getInstance().refresh();
    }

    @Override
    public void update(EventObject event)
    {
        if (event instanceof FunctionSelectedEvent)
        {
            FunDef functionData = ((FunctionSelectedEvent) event).getFunction();

            for (SingleFunctionContainer currentPanel : mFunctionContainerList)
            {
                if (functionData.getName().equals(currentPanel.getFunDef().getName()))
                {
                    currentPanel.setSelectedBackground(true);
                    mMainScrollPanel.getVerticalScrollBar().setValue(mFunctionContainerList.indexOf(currentPanel) * 75);
                    currentPanel.getNameInput().requestFocusInWindow();
                }
                else
                {
                    currentPanel.setSelectedBackground(false);
                }
            }

        }
        else if (event instanceof FunctionCreatedEvent)
        {
            refreshFunctionsContainerPanel();

            // Highlight and set scrollbar to selected function
            FunDef functionData = ((FunctionCreatedEvent) event).getFunction();

            for (SingleFunctionContainer currentPanel : mFunctionContainerList)
            {
                if (functionData.getName().equals(currentPanel.getFunDef().getName()))
                {
                    mMainScrollPanel.getVerticalScrollBar().setValue(mFunctionContainerList.indexOf(currentPanel) * 75);
                }
                else
                {
                    currentPanel.setSelectedBackground(false);
                }
            }

            // Look for function in list
            for (SingleFunctionContainer currentPanel : mFunctionContainerList)
            {
                if (functionData.getName().equals(currentPanel.getFunDef().getName()))
                {
                    currentPanel.getNameInput().requestFocusInWindow();
                    currentPanel.updateFunDef();
                }
                else
                {
                    currentPanel.setSelectedBackground(false);
                }
            }
        }
        else if (event instanceof FunctionModifiedEvent)
        {
            refreshFunctionsContainerPanel();

            FunDef functionData = ((FunctionModifiedEvent) event).getFunction();

            // Look for function in list
            for (SingleFunctionContainer currentPanel : mFunctionContainerList)
            {
                if (functionData.getName().equals(currentPanel.getFunDef().getName()))
                {
                    currentPanel.getNameInput().requestFocusInWindow();
                    currentPanel.updateFunDef();
                }
                else
                {
                    currentPanel.setSelectedBackground(false);
                }
            }
        }
        else if (event instanceof FunctionRemovedEvent)
        {
            String functionName = ((FunctionRemovedEvent) event).getFunction().getName();

            mUndoManager = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getUndoManager();
            mUndoManager.addEdit(new Edit(mSceneFlow.getUserCommandDefinitionAt(functionName), false));
            UndoAction.getInstance().refreshUndoState();
            RedoAction.getInstance().refreshRedoState();

//            mSceneFlow.getUserCommandDefinitionAt(functionName).setActive(false);
            mSceneFlow.removeUsrCmdDef(functionName);

            refreshFunctionsContainerPanel();
            EditorInstance.getInstance().refresh();
        }

    }

    public final void refresh()
    {
        // Refresh the visual appearance
    }

    private class Observable extends java.util.Observable
    {

        public void update(Object obj)
        {
            setChanged();
            notifyObservers(obj);
        }
    }

    public final void close()
    {
        // Remove All Observers
        //mObservable.deleteObservers();
        // TODO: Close the functions editor
    }

    /**
     *
     */
    private void launchFunctionCreatedEvent(FunDef funDef)
    {
        FunctionCreatedEvent ev = new FunctionCreatedEvent(this, funDef);
        mEventCaster.convey(ev);
    }
//    public void run() {
//        mUndoManager.addEdit(new Edit());
//        UndoAction.getInstance().refreshUndoState();
//        RedoAction.getInstance().refreshRedoState();
//    }

    private class Edit extends AbstractUndoableEdit
    {

        FunDef funDef;
        Boolean newFunction = false;

        public Edit(FunDef fd, Boolean newFun)
        {
            funDef = fd;
            newFunction = newFun;
        }

        @Override
        public void undo() throws CannotUndoException
        {
            if (newFunction)
            {
                unsetFunDef();
            }
            else
            {
                setFunDef();
            }
        }

        @Override
        public void redo() throws CannotRedoException
        {
            if (newFunction)
            {
                setFunDef();
            }
            else
            {
                unsetFunDef();
            }
        }

        private void setFunDef()
        {
            redoAddFunction(funDef);
//            mMainScrollPanel.repaint();

        }

        private void unsetFunDef()
        {
            undoDeleteFunction(funDef);
        }

        @Override
        public boolean canUndo()
        {
            return true;
        }

        @Override
        public boolean canRedo()
        {
            return true;
        }

        @Override
        public String getUndoPresentationName()
        {
            return "Undo Deletion Of Function";
        }

        @Override
        public String getRedoPresentationName()
        {
            return "Redo Deletion Of Function";
        }
    }
}
