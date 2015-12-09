package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.event.VariableChangedEvent;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.diagram.nodes.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.expression.AbstractExpression;
import de.dfki.vsm.model.sceneflow.command.expression.UnaryExpression;
import de.dfki.vsm.model.sceneflow.command.expression.constant.BoolLiteral;
import de.dfki.vsm.model.sceneflow.command.expression.constant.FloatLiteral;
import de.dfki.vsm.model.sceneflow.command.expression.constant.IntLiteral;
import de.dfki.vsm.model.sceneflow.command.expression.constant.ListRecord;
import de.dfki.vsm.model.sceneflow.command.expression.constant.StringLiteral;
import de.dfki.vsm.model.sceneflow.command.expression.constant.StructRecord;
import de.dfki.vsm.model.sceneflow.definition.VariableDefinition;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.WindowConstants;
import javax.swing.table.DefaultTableModel;

/**
 * @author Not me
 */
public class MonitorDialog extends JDialog implements  EventListener{
    private static MonitorDialog sSingeltonInstance = null;
    private JPanel               mMainPanel;
    private JPanel               mButtonsPanel;
    private CancelButton         mCancelButton;
    private OKButton             mOkButton;
    private JPanel               mWorkPanel;
    //private JList                mVariableList;
    private JTable               mVariableTable;
    private HintTextField           mInputTextField;
    private JScrollPane          mVariableScrollPane;
    private ArrayList<VariableDefinition>       mVarDefListData;
    private static EditorProject       mEditorProject;
    
    private MonitorDialog() {
        super(EditorInstance.getInstance(), "Run Monitor", true);
        EditorInstance.getInstance().addEscapeListener(this);
        mEditorProject = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        initComponents();
        // Add the sceneflowtoolbar to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    public static MonitorDialog getInstance() {
        mEditorProject = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new MonitorDialog();
        }
        return sSingeltonInstance;
    }

    public void init(SceneFlow sceneFlow) {}
    
    public void resetView()
    {
        remove(mMainPanel);
        mMainPanel = new JPanel(null);
        initWorkPanel();
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(mWorkPanel);
        add(mMainPanel);
    }
    private void initWorkPanel() {
        mWorkPanel = new JPanel(null);
        mWorkPanel.setBounds(0, 0, 400, 400);
        mWorkPanel.setBorder(BorderFactory.createLoweredBevelBorder());
        
        initVariableList();
        mVariableScrollPane = new JScrollPane(mVariableTable);
        mVariableScrollPane.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        mVariableScrollPane.setBounds(20, 10, 360, 300);
        mInputTextField = new HintTextField("Enter new value");
        mInputTextField.setBounds(20, 320, 360, 30);
        mWorkPanel.add(mVariableScrollPane);
        if(!RunTimeInstance.getInstance().isRunning(mEditorProject))
        {
            mInputTextField.setEnabled(false);
        }
        mWorkPanel.add(mInputTextField);
    }

    private boolean process() {
        int selectedIndex = mVariableTable.getSelectedRow();
        
        if (selectedIndex != -1) {
            VariableDefinition           varDef      = mVarDefListData.get(selectedIndex);
            java.lang.String inputString = mInputTextField.getText().trim();
            
            try {
                _SFSLParser_.parseResultType = _SFSLParser_.EXPRESSION;
                _SFSLParser_.run(inputString);

                AbstractExpression exp = _SFSLParser_.expResult;
                
                //TODO UNARY EXPRESSION MUST BE SEPARATED FOR EACH DIFFERENT VALUE (FLOAT, INT, DOUBLE)
                if ((exp != null) &&!_SFSLParser_.errorFlag) {
                    if (exp instanceof BoolLiteral) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((BoolLiteral) exp).getValue());
                    } else if (exp instanceof IntLiteral) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((IntLiteral) exp).getValue());
                    } else if (exp instanceof UnaryExpression) {
                        if ( ((UnaryExpression)exp).getExp() instanceof IntLiteral) {
                            return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1*((IntLiteral)((UnaryExpression) exp).getExp()).getValue());
                        }
                        if ( ((UnaryExpression)exp).getExp() instanceof FloatLiteral) {
                            return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1*((FloatLiteral)((UnaryExpression) exp).getExp()).getValue());
                        }
                        
                    } else if (exp instanceof FloatLiteral) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((FloatLiteral) exp).getValue());
                    } else if (exp instanceof StringLiteral) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((StringLiteral) exp).getValue());
                    } else if (exp instanceof ListRecord) {
                        //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);

                        // Evaluator eval = interpreter.getEvaluator();
                        // Environment env = interpreter.getEnvironment();
                        // return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), eval.evaluate(exp, env));
                    } else if (exp instanceof StructRecord) {
                        //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);
                    } else {
                        System.out.println("Expression could not be parsed");
                        //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);
                    }
                }
            } catch (Exception e) {
                System.err.println(e.toString());
                for (StackTraceElement st : e.getStackTrace()) {
                    System.out.println(st);
                }
            }
        }

        return false;
    }

    private void initComponents() {
        initWorkPanel();
        mButtonsPanel = new JPanel(null);
        mButtonsPanel.setBounds(0, 400, 400, 40);
        mOkButton = new OKButton();
        mOkButton.setBounds(205, 0, 125, 30);
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                boolean varAssigned = process();
                dispose();
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setBounds(50, 0, 125, 30);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                dispose();
            }
        });
        mButtonsPanel.add(mOkButton);
        mButtonsPanel.add(mCancelButton);
        mMainPanel = new JPanel(null);
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(mWorkPanel);
        add(mMainPanel);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setResizable(false);
        pack();
        setSize(new Dimension(400 + getInsets().left + getInsets().right, 440 + getInsets().top + getInsets().bottom));
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                    getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
        mOkButton.requestFocus();
    }

    private void initVariableList() {
        mVarDefListData = mEditorProject.getSceneFlow().getCopyOfVarDefList();
        java.lang.String listofVars[][] = new java.lang.String[mVarDefListData.size()][2];
        java.lang.String[] listOfColumns = {"Variable", "Value"};
        int counter = 0;
        for (VariableDefinition varDef : mVarDefListData) {
            java.lang.String[] tempString = { varDef.getName(), varDef.getExp().toString(), varDef.getFormattedSyntax()};
            listofVars[counter] =  tempString;
            counter++;
        }
        mVariableTable = new JTable(new DefaultTableModel(listofVars, listOfColumns)
        {

            @Override
            public boolean isCellEditable(int i, int i1) {
                return false;
            }
            
        });
    }

    @Override
    public void update(EventObject event) {
            
        if( event instanceof VariableChangedEvent)
        {
             for (int i = 0; i < mVariableTable.getRowCount(); i++) {
                 
                    if(mVariableTable.getValueAt(i, 0).equals(((VariableChangedEvent)event).getVarValue().getFirst()))
                    {
                        java.lang.String value = (((VariableChangedEvent)event).getVarValue().getSecond());
                        value = value.replace("#c#", "");                     
                        mVariableTable.setValueAt(value, i, 1);
                    }
            }
        }
    }
  }
