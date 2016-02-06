package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------
import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.event.VariableChangedEvent;
import de.dfki.vsm.editor.util.HintTextField;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.UnaryExp;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Float;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Int;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.List;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.String;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Struct;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;

//~--- JDK imports ------------------------------------------------------------
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.text.AttributedString;

import java.util.Vector;
import javax.swing.*;
import javax.swing.table.DefaultTableModel;

/**
 * @author Not me
 */
public class MonitorDialog extends JDialog implements EventListener
{

    private static MonitorDialog sSingeltonInstance = null;
    private JPanel mMainPanel;
    private JPanel mButtonsPanel;
    private CancelButton mCancelButton;
    private OKButton mOkButton;
    private JPanel mWorkPanel;
    //private JList                 mVariableList;
    private JTable mGlobalVariableTable;
    private JTable mLocalVariableTable;
    private HintTextField mInputTextField;
    private JScrollPane mVariableScrollPane;
    private Vector<VarDef> mGlobalVarDefListData;
    private Vector<VarDef> mLocalVarDefListData;
    private static EditorProject mEditorProject;
    private JLabel errorMsg;

    private MonitorDialog()
    {
        super(EditorInstance.getInstance(), "Run Monitor", true);
        EditorInstance.getInstance().addEscapeListener(this);
        mEditorProject = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        initComponents();
        // Add the sceneflowtoolbar to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    public static MonitorDialog getInstance()
    {
        mEditorProject = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new MonitorDialog();
        }
        return sSingeltonInstance;
    }

    public void init(SceneFlow sceneFlow)
    {
    }

    public void resetView()
    {
        remove(mMainPanel);
        mMainPanel = new JPanel(null);
        initWorkPanel();
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(mWorkPanel);
        add(mMainPanel);
    }

    private void initWorkPanel()
    {
        mWorkPanel = new JPanel(null);
        mWorkPanel.setBounds(0, 0, 400, 400);
        mWorkPanel.setBorder(BorderFactory.createLoweredBevelBorder());

        initGlobalVariableList();
        initLocalVariableList();

        // errorMsg.setForeground(Color.white);
        errorMsg = new JLabel("");
        errorMsg.setBounds(20, 350, 360, 30);

        //VAR BOX
        Box varBox = Box.createVerticalBox();
        varBox.add(Box.createVerticalStrut(20));
        varBox.add(new JLabel("Global Variables"));
        varBox.add(mGlobalVariableTable);
        varBox.add(Box.createVerticalStrut(20));
        varBox.add(new JLabel("Local Variables"));
        varBox.add(mLocalVariableTable);

        mVariableScrollPane = new JScrollPane(varBox);
        mVariableScrollPane.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        mVariableScrollPane.setBounds(20, 10, 360, 300);
        mInputTextField = new HintTextField("Enter new value");
        mInputTextField.setBounds(20, 320, 360, 30);
        mWorkPanel.add(mVariableScrollPane);
        if (!RunTimeInstance.getInstance().isRunning(mEditorProject)) {
            mInputTextField.setEnabled(false);
        }
        mInputTextField.addKeyListener(new KeyAdapter()
        {
            @Override
            public void keyReleased(KeyEvent ke)
            {
                int selectedGlobalVarsIndex = mGlobalVariableTable.getSelectedRow();
                int selectedLocalVarsIndex = mLocalVariableTable.getSelectedRow();
                if (selectedGlobalVarsIndex == -1 && selectedLocalVarsIndex == -1) {
                    mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
                    errorMsg.setText("Please select one variable from the variable list");

                }
                else {
                    mInputTextField.setBorder(BorderFactory.createEmptyBorder());
                    errorMsg.setText("");

                }
            }
        });

        mInputTextField.addActionListener(new ActionListener()
        {
            @Override
            public void actionPerformed(ActionEvent e)
            {
                okActionPerformed();
            }
        });

        mWorkPanel.add(errorMsg);
        mWorkPanel.add(mInputTextField);
    }

    private boolean process()
    {
        int selectedGlobalVarsIndex = mGlobalVariableTable.getSelectedRow();
        int selectedLocalVarsIndex = mLocalVariableTable.getSelectedRow();

        VarDef varDef;
        java.lang.String inputString;

        if (selectedGlobalVarsIndex != -1) {
            varDef = mGlobalVarDefListData.get(selectedGlobalVarsIndex);
            inputString = mInputTextField.getText().trim();
            return updateAVariable(varDef, inputString);
        }
        if (selectedLocalVarsIndex != -1) {
            varDef = mLocalVarDefListData.get(selectedLocalVarsIndex);
            inputString = mInputTextField.getText().trim();
            EditorProject selectedEP = EditorInstance.getInstance().getSelectedProjectEditor().getEditorProject();
            return updateAVariable(varDef, inputString);
        }
        return false;
    }

    public boolean updateAVariable(VarDef varDef, java.lang.String value)
    {
        try {
            _SFSLParser_.parseResultType = _SFSLParser_.EXP;
            _SFSLParser_.run(value);

            Expression exp = _SFSLParser_.expResult;

            //TODO UNARY EXPRESSION MUST BE SEPARATED FOR EACH DIFFERENT VALUE (FLOAT, INT, DOUBLE)
            if ((exp != null) && !_SFSLParser_.errorFlag) {
                if (exp instanceof Bool) {
                    return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Bool) exp).getValue());
                }
                else if (exp instanceof Int) {
                    return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Int) exp).getValue());
                }
                else if (exp instanceof UnaryExp) {
                    if (((UnaryExp) exp).getExp() instanceof Int) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1 * ((Int) ((UnaryExp) exp).getExp()).getValue());
                    }
                    if (((UnaryExp) exp).getExp() instanceof Float) {
                        return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1 * ((Float) ((UnaryExp) exp).getExp()).getValue());
                    }

                }
                else if (exp instanceof Float) {
                    return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Float) exp).getValue());
                }
                else if (exp instanceof String) {
                    return RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((String) exp).getValue());
                }
                else if (exp instanceof List) {
                    //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);

                    // Evaluator eval = interpreter.getEvaluator();
                    // Environment env = interpreter.getEnvironment();
                    // return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), eval.evaluate(exp, env));
                }
                else if (exp instanceof Struct) {
                    //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);
                }
                else {
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

        return false;
    }

    private boolean validateValues()
    {
        int selectedGlobalVarsIndex = mGlobalVariableTable.getSelectedRow();
        int selectedLocalVarsIndex = mLocalVariableTable.getSelectedRow();

        if (mInputTextField.getText().length() == 0 || (selectedLocalVarsIndex == -1 && selectedGlobalVarsIndex == -1)) {
            mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
            errorMsg.setForeground(Color.red);
            return false;
        }
        mInputTextField.setBorder(BorderFactory.createEmptyBorder());
        errorMsg.setText("");
        errorMsg.setForeground(Color.white);
        return true;
    }

    protected void okActionPerformed()
    {
        if (validateValues() == true) {
            boolean varAssigned = process();
            dispose();
        }
    }

    private void initComponents()
    {
        initWorkPanel();
        mButtonsPanel = new JPanel(null);
        mButtonsPanel.setBounds(0, 400, 400, 40);
        mOkButton = new OKButton();
        mOkButton.setBounds(205, 0, 125, 30);
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
                okActionPerformed();
            }
        });
        mCancelButton = new CancelButton();
        mCancelButton.setBounds(50, 0, 125, 30);
        mCancelButton.addMouseListener(new java.awt.event.MouseAdapter()
        {
            public void mouseClicked(java.awt.event.MouseEvent evt)
            {
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

    private void initGlobalVariableList()
    {
        mGlobalVarDefListData = mEditorProject.getSceneFlow().getCopyOfVarDefList();
        java.lang.String listofGlobalVars[][] = new java.lang.String[mGlobalVarDefListData.size()][2];
        java.lang.String[] listOfColumns = {"Variable", "Value"};
        int counter = 0;
        for (VarDef varDef : mGlobalVarDefListData) {
            java.lang.String[] tempString = {varDef.getName(), varDef.getExp().toString(), varDef.getFormattedSyntax()};
            listofGlobalVars[counter] = tempString;
            counter++;
        }
        mGlobalVariableTable = new JTable(new DefaultTableModel(listofGlobalVars, listOfColumns)
        {

            @Override
            public boolean isCellEditable(int i, int i1)
            {
                return false;
            }

        });
    }

    private void initLocalVariableList()
    {
        mLocalVarDefListData = EditorInstance.getInstance().getSelectedProjectEditor().getSceneFlowEditor().getSceneFlowManager().getCurrentActiveSuperNode().getCopyOfVarDefList();
        java.lang.String listofVars[][] = new java.lang.String[mLocalVarDefListData.size()][2];
        java.lang.String[] listOfColumns
                = {
                    "Variable", "Value"
                };
        int counter = 0;
        for (VarDef varDef : mLocalVarDefListData) {
            java.lang.String[] tempString
                    = {
                        varDef.getName(), varDef.getExp().toString(), varDef.getFormattedSyntax()
                    };
            listofVars[counter] = tempString;
            counter++;
        }
        mLocalVariableTable = new JTable(new DefaultTableModel(listofVars, listOfColumns)
        {

            @Override
            public boolean isCellEditable(int i, int i1)
            {
                return false;
            }

        });
    }

    @Override
    public void update(EventObject event)
    {

        if (event instanceof VariableChangedEvent) {
            for (int i = 0; i < mGlobalVariableTable.getRowCount(); i++) {

                if (mGlobalVariableTable.getValueAt(i, 0).equals(((VariableChangedEvent) event).getVarValue().getFirst())) {
                    java.lang.String value = (((VariableChangedEvent) event).getVarValue().getSecond());
                    value = value.replace("#c#", "");
                    mGlobalVariableTable.setValueAt(value, i, 1);
                }
            }
        }
    }
}
