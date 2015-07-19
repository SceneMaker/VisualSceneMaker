package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.CancelButton;
import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.command.expression.Expression;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Bool;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Float;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Int;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.List;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.String;
import de.dfki.vsm.model.sceneflow.command.expression.condition.constant.Struct;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.RunTime;
import de.dfki.vsm.sfsl.parser._SFSLParser_;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Dimension;

import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.WindowConstants;

/**
 * @author Gregor Mehlmann
 */
public class MonitorDialog extends JDialog {
    private static MonitorDialog sSingeltonInstance = null;
    private JPanel               mMainPanel;
    private JPanel               mButtonsPanel;
    private CancelButton         mCancelButton;
    private OKButton             mOkButton;
    private JPanel               mWorkPanel;
    private JList                mVariableList;
    private JTextField           mInputTextField;
    private JScrollPane          mVariableScrollPane;
    private Vector<VarDef>       mVarDefListData;
    private final SceneFlow      mSceneFlow;

    private MonitorDialog() {
        super(Editor.getInstance(), "Variable Manager", true);
        this.setIconImage(ResourceLoader.loadImageIcon("/res/img/toolbar_icons/stack_icon.png").getImage());
        mSceneFlow = Editor.getInstance().getProjectEditorList().getSelectedProject().getSceneFlow();
        initComponents();
        //initVariableList(); Now this init is being called from the button calling the monitor -- by M. Fallas 07 2015 
    }

    public static MonitorDialog getInstance() {
        if (sSingeltonInstance == null) {
            sSingeltonInstance = new MonitorDialog();
        }
        
        return sSingeltonInstance;
    }

    public void init(SceneFlow sceneFlow) {}

    private void initWorkPanel() {
        mWorkPanel = new JPanel(null);
        mWorkPanel.setBounds(0, 0, 400, 400);
        mWorkPanel.setBorder(BorderFactory.createLoweredBevelBorder());
        mVariableList       = new JList(new DefaultListModel());
        mVariableScrollPane = new JScrollPane(mVariableList);
        mVariableScrollPane.setBounds(20, 10, 360, 300);
        mInputTextField = new JTextField();
        mInputTextField.setBounds(20, 320, 360, 30);
        mWorkPanel.add(mVariableScrollPane);
        mWorkPanel.add(mInputTextField);
    }

    private boolean process() {
        int selectedIndex = mVariableList.getSelectedIndex();

        if (selectedIndex != -1) {
            VarDef           varDef      = mVarDefListData.get(selectedIndex);
            java.lang.String inputString = mInputTextField.getText().trim();

            try {
                _SFSLParser_.parseResultType = _SFSLParser_.EXP;
                _SFSLParser_.run(inputString);

                Expression exp = _SFSLParser_.expResult;

                if ((exp != null) &&!_SFSLParser_.errorFlag) {
                    if (exp instanceof Bool) {
                        return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), ((Bool) exp).getValue());
                    } else if (exp instanceof Int) {
                        return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), ((Int) exp).getValue());
                    } else if (exp instanceof Float) {
                        return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(),
                                ((Float) exp).getValue());
                    } else if (exp instanceof String) {
                        return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(),
                                ((String) exp).getValue());
                    } else if (exp instanceof List) {
                        return RunTime.getInstance().setVariable(mSceneFlow, mSceneFlow.getId(), varDef.getName(), exp);
                        // Evaluator eval = interpreter.getEvaluator();
                        // Environment env = interpreter.getEnvironment();
                        // return RunTime.getInstance().setVariable(mSceneFlow, varDef.getName(), eval.evaluate(exp, env));
                    } else if (exp instanceof Struct) {
                        return RunTime.getInstance().setVariable(mSceneFlow, mSceneFlow.getId(), varDef.getName(), exp);
                    } else {
                        return RunTime.getInstance().setVariable(mSceneFlow, mSceneFlow.getId(), varDef.getName(), exp);
                    }
                }
            } catch (Exception e) {
                System.err.println(e.toString());
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
    }

    public void initVariableList() {
        mVarDefListData = mSceneFlow.getCopyOfVarDefList();
        ((DefaultListModel) mVariableList.getModel()).removeAllElements();
        for (VarDef varDef : mVarDefListData) {
            ((DefaultListModel) mVariableList.getModel()).addElement(varDef.getType() + " " + varDef.getName());
        }
    }
}
