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
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.ListRecord;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.StringLiteral;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.StructRecord;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
//import de.dfki.vsm.runtime.RunTimeInstance;
import de.dfki.vsm.model.sceneflow.ChartParser;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.jpl.JPLEngine;
import de.dfki.vsm.util.jpl.JPLResult;

//~--- JDK imports ------------------------------------------------------------
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import javax.swing.*;
import javax.swing.table.DefaultTableModel;

/**
 * @author Gregor Mehlmann
 */
public class MonitorDialog extends JDialog implements EventListener {

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
    private HintTextField mQueryTextField;
    private JScrollPane mVariableScrollPane;
    private ArrayList<VarDef> mGlobalVarDefListData;
    private ArrayList<VarDef> mLocalVarDefListData;
    private static EditorProject mEditorProject;
    private JLabel errorMsg;

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

    public void init(SceneFlow sceneFlow) {
    }

    public void resetView() {
        remove(mMainPanel);
        mMainPanel = new JPanel(null);
        initWorkPanel();
        mMainPanel.add(mButtonsPanel);
        mMainPanel.add(mWorkPanel);
        add(mMainPanel);
    }

    private void initWorkPanel() {
        mWorkPanel = new JPanel(null);
        mWorkPanel.setBounds(0, 0, 400, 460);
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
        mWorkPanel.add(mVariableScrollPane);

        mInputTextField = new HintTextField("Enter new value");
        mInputTextField.setBounds(20, 320, 360, 30);
        if (!mEditorProject.isRunning()) {
            mInputTextField.setEnabled(false);
        }
        mInputTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                int selectedGlobalVarsIndex = mGlobalVariableTable.getSelectedRow();
                int selectedLocalVarsIndex = mLocalVariableTable.getSelectedRow();
                if (selectedGlobalVarsIndex == -1 && selectedLocalVarsIndex == -1) {
                    mInputTextField.setBorder(BorderFactory.createLineBorder(Color.red));
                    errorMsg.setText("Please select one variable from the variable list");

                } else {
                    mInputTextField.setBorder(BorderFactory.createEmptyBorder());
                    errorMsg.setText("");

                }
            }
        });

        mInputTextField.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });

        //Key listener need to gain focus on the text field
        //KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {
        //  @Override
        // public boolean dispatchKeyEvent(KeyEvent ke) {
        //boolean keyHandled = false;
        //    if (ke.getID() == KeyEvent.KEY_PRESSED) {
        //        if (!mInputTextField.hasFocus()) {
        //           mInputTextField.setText(mInputTextField.getText() + ke.getKeyChar());
        //           mInputTextField.requestFocus();
        //      }
        //   }
        //   return false;
        //}
        //});
        mQueryTextField = new HintTextField("Enter new query");
        mQueryTextField.setBounds(20, 380, 360, 30);
        if (!mEditorProject.isRunning()) {
            mQueryTextField.setEnabled(false);
        }
        mQueryTextField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyReleased(KeyEvent ke) {
                //
            }
        });
        mQueryTextField.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                okActionPerformed();
            }
        });
        //KeyboardFocusManager.getCurrentKeyboardFocusManager().addKeyEventDispatcher(new KeyEventDispatcher() {

        //  @Override
        //  public boolean dispatchKeyEvent(final KeyEvent key) {
        //boolean keyHandled = false;
        //if (key.getID() == KeyEvent.KEY_PRESSED) {
        //    if (!mQueryTextField.hasFocus()) {
        //        mQueryTextField.setText(mQueryTextField.getText() + key.getKeyChar());
        //        mQueryTextField.requestFocus();
        //    }
        //}
        //return false;
        //  }
        //});
        mWorkPanel.add(errorMsg);
        mWorkPanel.add(mInputTextField);
        mWorkPanel.add(mQueryTextField);
    }

    private boolean process() {
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

    public boolean updateAVariable(VarDef varDef, java.lang.String value) {
        try {
            ChartParser.parseResultType = ChartParser.EXP;
            ChartParser.run(value);

            Expression exp = ChartParser.expResult;

            //TODO UNARY EXPRESSION MUST BE SEPARATED FOR EACH DIFFERENT VALUE (FLOAT, INT, DOUBLE)
            if ((exp != null) && !ChartParser.errorFlag) {
                if (exp instanceof Bool) {
                    return mEditorProject.setVariable(varDef.getName(), ((Bool) exp).getValue());
                    //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Bool) exp).getValue());
                } else if (exp instanceof Int) {
                    return mEditorProject.setVariable(varDef.getName(), ((Int) exp).getValue());
                    //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Int) exp).getValue());
                } else if (exp instanceof UnaryExp) {
                    if (((UnaryExp) exp).getExp() instanceof Int) {
                        return mEditorProject.setVariable(varDef.getName(), -1 * ((Int) ((UnaryExp) exp).getExp()).getValue());
                        //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1 * ((Int) ((UnaryExp) exp).getExp()).getValue());
                    }
                    if (((UnaryExp) exp).getExp() instanceof Float) {
                        return mEditorProject.setVariable(varDef.getName(), -1 * ((Float) ((UnaryExp) exp).getExp()).getValue());
                        //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), -1 * ((Float) ((UnaryExp) exp).getExp()).getValue());
                    }

                } else if (exp instanceof Float) {
                    return mEditorProject.setVariable(varDef.getName(), ((Float) exp).getValue());
                    //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((Float) exp).getValue());
                } else if (exp instanceof StringLiteral) {
                    return mEditorProject.setVariable(varDef.getName(), ((StringLiteral) exp).getValue());
                    //RunTimeInstance.getInstance().setVariable(mEditorProject, varDef.getName(), ((StringLiteral) exp).getValue());
                } else if (exp instanceof ListRecord) {
                    System.out.println("ListRecord could not be parsed");
                    //return RunTimeInstance.getInstance().setVariable(mEditorProject,  varDef.getName(), exp);

                    // Evaluator eval = interpreter.getEvaluator();
                    // Environment env = interpreter.getEnvironment();
                    // return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), eval.evaluate(exp, env));
                } else if (exp instanceof StructRecord) {
                    System.out.println("StructRecord could not be parsed");
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

        return false;
    }

    private boolean validateValues() {
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

    protected void okActionPerformed() {
        final String querystr = mQueryTextField.getText().trim();
        final String valuestr = mInputTextField.getText().trim();

        if (valuestr.isEmpty() && !querystr.isEmpty()) {
            // Execute a query
            final JPLResult result = JPLEngine.query(querystr);
            dispose();
        } else if (querystr.isEmpty() && !valuestr.isEmpty()) {
            // Set a variable
            if (validateValues()) {
                final boolean variable = process();
                dispose();
            }
        }
    }

    private void initComponents() {
        initWorkPanel();
        mButtonsPanel = new JPanel(null);
        mButtonsPanel.setBounds(0, 460, 400, 40);
        mOkButton = new OKButton();
        mOkButton.setBounds(205, 0, 125, 30);
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                okActionPerformed();
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
        setSize(new Dimension(400 + getInsets().left + getInsets().right, 500 + getInsets().top + getInsets().bottom));
        setLocation(getParent().getLocation().x + (getParent().getWidth() - getWidth()) / 2,
                getParent().getLocation().y + (getParent().getHeight() - getHeight()) / 2);
        mOkButton.requestFocus();
    }

    private void initGlobalVariableList() {
        mGlobalVarDefListData = mEditorProject.getSceneFlow().getCopyOfVarDefList();
        java.lang.String listofGlobalVars[][] = new java.lang.String[mGlobalVarDefListData.size()][2];
        java.lang.String[] listOfColumns
                = {
                    "Variable", "Value"
                };
        int counter = 0;
        for (VarDef varDef : mGlobalVarDefListData) {
            java.lang.String[] tempString
                    = {
                        varDef.getName(), varDef.getExp().toString(), varDef.getFormattedSyntax()
                    };
            listofGlobalVars[counter] = tempString;
            counter++;
        }
        mGlobalVariableTable = new JTable(new DefaultTableModel(listofGlobalVars, listOfColumns) {

            @Override
            public boolean isCellEditable(int i, int i1) {
                return false;
            }

        });
        mGlobalVariableTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent me) {
                super.mouseClicked(me);
                mLocalVariableTable.getSelectionModel().clearSelection();
            }

        });
    }

    private void initLocalVariableList() {
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
        mLocalVariableTable = new JTable(new DefaultTableModel(listofVars, listOfColumns) {

            @Override
            public boolean isCellEditable(int i, int i1) {
                return false;
            }

        });
        mLocalVariableTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent me) {
                super.mouseClicked(me);
                mGlobalVariableTable.getSelectionModel().clearSelection();
            }

        });
    }

    @Override
    public void update(EventObject event) {

        if (event instanceof VariableChangedEvent) {
            for (int i = 0; i < mGlobalVariableTable.getRowCount(); i++) {

                if (mGlobalVariableTable.getValueAt(i, 0).equals(((VariableChangedEvent) event).getVarValue().getFirst())) {
                    java.lang.String value = (((VariableChangedEvent) event).getVarValue().getSecond());
                    value = value.replace("#c#", "");
                    mGlobalVariableTable.setValueAt(value, i, 1);
                }
            }
            for (int i = 0; i < mLocalVariableTable.getRowCount(); i++) {

                if (mLocalVariableTable.getValueAt(i, 0).equals(((VariableChangedEvent) event).getVarValue().getFirst())) {
                    java.lang.String value = (((VariableChangedEvent) event).getVarValue().getSecond());
                    value = value.replace("#c#", "");
                    mLocalVariableTable.setValueAt(value, i, 1);
                }
            }
        }
    }
}
