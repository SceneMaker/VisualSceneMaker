package de.dfki.vsm.editor.script;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.dialog.DialogActAttributes;
import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observer;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * @author Sergio Soto
 */
class DialogActEditor extends JScrollPane implements EventListener, Observer {
    private final Observable         mObservable = new DialogActEditor.Observable();
    // Main Pane Containers
    private JSplitPane               mMainSplitPane;
    private JScrollPane              mDialogActsPanel;
    private JPanel                   mUtterancePanel;
    // Left Panel
    private JPanel                   mDATitlePanel;
    private JLabel                   mDATitle;
    private JPanel                   mDAContainerPanel;
    private JPanel                   mPhasesListPanel;
    // Right Panel
    private JPanel                   mUtteranceTitlePanel;
    private JLabel                   mUtteranceTitle;
    private JButton                  mTextButton;
    private JButton                  mFMLButton;
    private JScrollPane              mUtteranceTextPanel;
    private JTextArea                mUtteranceTextArea;
    private JTextArea                mFMLTextArea;
    // 
    private final ProjectData        mProject;
    private final DialogActInterface mDialogAct;

    private final List<JList> mDAJLists;
    private List<DialogAct> mDialogActList;
    
            
            
            
    public DialogActEditor(ProjectData project) {
        mProject   = project;
        mDialogAct = mProject.getDialogAct();
        setMinimumSize(new Dimension(0, 200));
        mDAJLists = new ArrayList<>();
        mDialogActList = new ArrayList<>();
        
        initComponents();

        // Add the element editor to the event multicaster
        EventCaster.getInstance().append(this);
    }

    private void initComponents() {
        // init components
        initDialogActsPanel();
        initUtterancePanel();
        // set up gui
        mMainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, mDialogActsPanel, mUtterancePanel);
        mMainSplitPane.setResizeWeight(0.2);
        mMainSplitPane.setDividerSize(1);
        mMainSplitPane.setContinuousLayout(true);
        setViewportView(mMainSplitPane);
    }

    private void initDialogActsPanel() {
        // Title bar
        mDATitle = new JLabel("DialogueActs & Phases");
        mDATitle.setBackground(Color.DARK_GRAY);
        mDATitle.setForeground(Color.WHITE);
        mDATitlePanel = new JPanel();
        mDATitlePanel.setBackground(Color.LIGHT_GRAY);
        mDATitlePanel.setMaximumSize(new Dimension(20000, 20)); // TODO: change that 2000 to actual width   
        mDATitlePanel.add(Box.createRigidArea(new Dimension(10, 0)));
        mDATitlePanel.add(mDATitle);
        mDATitlePanel.setBackground(Color.DARK_GRAY);
        mDATitlePanel.setForeground(Color.WHITE);

        
        // Fill with phases and their corresponding dialog acts
        mPhasesListPanel = new JPanel();
        mPhasesListPanel.setLayout(new BoxLayout(mPhasesListPanel, BoxLayout.Y_AXIS));

        for (String var : mDialogAct.getDialogueActPhases()) {
            
          
          
            mPhasesListPanel.add(createPhase(var));
        }

        mDAContainerPanel = new JPanel();
        mDAContainerPanel.setLayout(new BoxLayout(mDAContainerPanel, BoxLayout.Y_AXIS));
        mDAContainerPanel.add(mDATitlePanel);
        mDAContainerPanel.add(mPhasesListPanel);
        mDialogActsPanel = new JScrollPane(mDAContainerPanel);
         mDialogActsPanel.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        //mDialogActsPanel.setViewportView(mDAContainerPanel);
       
    }
    
    /**
     *
     */
    private JPanel createPhase(String phase) {
        
        JPanel phaseContainer = new JPanel();
        phaseContainer.setLayout(new BoxLayout(phaseContainer, BoxLayout.X_AXIS));

        
        
        

        // Fill list with all dialog acts from phase
         DefaultListModel dialogActListModel = new DefaultListModel();
        for (String var : mDialogAct.getDialogueActs(phase)) {
            dialogActListModel.addElement(var);
           // mDialogActList.add(new DialogAct(var, phase));
        }
        final JList dialogActList = new JList(dialogActListModel);
        mDAJLists.add(dialogActList);

        dialogActList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        
        if ((mDialogAct.getDialogueActPhases().indexOf(phase) % 2)==0) 
        { 
            phaseContainer.setBackground(Color.LIGHT_GRAY);
            dialogActList.setBackground(Color.LIGHT_GRAY);
        }
        else{
            dialogActList.setBackground(UIManager.getColor("TableHeader.background"));
        }
        // 
        //dialogActList.setBackground(UIManager.getColor("TableHeader.background"));
        dialogActList.setSelectionBackground(Color.DARK_GRAY);
        dialogActList.setSelectionForeground(Color.WHITE);
        dialogActList.setOpaque(true);
        
        Border emptyBorder = BorderFactory.createEmptyBorder();
        dialogActList.setBorder(emptyBorder);

        dialogActList.setVisibleRowCount(3);
        dialogActList.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
               
                 
                for(JList i: mDAJLists){
                    if(!(i.equals(e.getSource()))){
                        i.clearSelection();  
                    }
                }
                
                Map<String, String> mAttributeValueMap = new HashMap();                
                for(String atttribute: mDialogAct.getNLGAttributes()){
                    mAttributeValueMap.put(atttribute, getAttributeSelectedValue(atttribute));
                }
                     
                mUtteranceTextArea.setText(mDialogAct.getUtterances(dialogActList.getSelectedValue().toString(),mAttributeValueMap));
                mFMLTextArea.setText(mDialogAct.getFMLCommands(dialogActList.getSelectedValue().toString(),mAttributeValueMap).toString());
                
            }

           
        });
        dialogActList.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
                JList list = (JList) evt.getSource();
                
                
                if (evt.getClickCount() == 2) {
                   

                    // mUtteranceTextArea.setText("DoubleClick");
                    // new FunDefDialog(null);
                    // attributes = new de.dfki.vsm.editor.dialog.DialogActAttributes();
                    DialogActAttributes tedgeDialog = new DialogActAttributes(mDialogAct);

                    tedgeDialog.run();
                }
            }
        });

        JScrollPane listScrollPane = new JScrollPane(dialogActList);
        listScrollPane.setPreferredSize(new Dimension(10,22*dialogActListModel.getSize()));
        listScrollPane.setBorder(emptyBorder);
        
        JPanel      daContainer    = new JPanel();
        daContainer.setLayout(new BoxLayout(daContainer, BoxLayout.Y_AXIS));

        JLabel phaseLabel = new JLabel(phase);
        
    
        phaseLabel.setPreferredSize(new Dimension(110, 20));
        phaseContainer.add(listScrollPane);
        phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));
        phaseContainer.add(phaseLabel);
        phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));

        return phaseContainer;
    }
    
    private String getAttributeSelectedValue(String atttribute) {
        // TODO
        System.out.println("todo");
        return "pending";
    }

    private void initUtterancePanel() {
        mUtteranceTitle = new JLabel("Utterance Text/ FML Command");
        mUtteranceTitle.setBackground(Color.DARK_GRAY);
        mUtteranceTitle.setForeground(Color.WHITE);

        
        // Create Text Button
        mTextButton = new JButton("Text");
        mTextButton.setBorderPainted(false);
        mTextButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {

                // Do something
                mFMLTextArea.setVisible(false);
                mUtteranceTextArea.setVisible(true);
            }
        });

        // Create FML Button
        mFMLButton = new JButton("FML");
        mFMLButton.setBorderPainted(false);
        mFMLButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                mFMLTextArea.setVisible(true);
                mUtteranceTextArea.setVisible(false);
                // Do something
            }
        });
        mUtteranceTitlePanel = new JPanel();
        mUtteranceTitlePanel.setBackground(Color.DARK_GRAY);
        mUtteranceTitlePanel.setLayout(new BoxLayout(mUtteranceTitlePanel, BoxLayout.X_AXIS));
        mUtteranceTitlePanel.setMaximumSize(new Dimension(20000, 20));    // TODO: change that 2000 to actual width
        mUtteranceTitlePanel.add(Box.createRigidArea(new Dimension(10, 0)));
        mUtteranceTitlePanel.add(mUtteranceTitle);
        mUtteranceTitlePanel.add(Box.createHorizontalGlue());
        mUtteranceTitlePanel.add(mTextButton);
        mUtteranceTitlePanel.add(mFMLButton);
        mUtteranceTextPanel = new JScrollPane();
        mUtteranceTextPanel.setBorder(BorderFactory.createEmptyBorder());
        mUtteranceTextArea = new JTextArea();        
        mFMLTextArea = new JTextArea();
        
        JPanel textArea = new JPanel();
         textArea.setLayout(new BoxLayout(textArea, BoxLayout.Y_AXIS));
       
        textArea.add(mUtteranceTextArea);
        textArea.add(mFMLTextArea);
        mFMLTextArea.setVisible(false);
        
        mUtteranceTextPanel.setViewportView(textArea);
        mUtterancePanel = new JPanel();
        mUtterancePanel.setLayout(new BoxLayout(mUtterancePanel, BoxLayout.Y_AXIS));
        mUtterancePanel.add(mUtteranceTitlePanel);
        mUtterancePanel.add(mUtteranceTextPanel);
        mUtterancePanel.setVisible(true);
    }

    

    /**
     *
     */
    @Override
    public void update(EventObject event) {}

    /**
     *
     */
    @Override
    public void update(java.util.Observable obs, Object obj) {
        mObservable.update(obj);
    }

    private static class Observable extends java.util.Observable {
        public Observable() {}

        public void update(Object obj) {
            setChanged();
            notifyObservers(obj);
        }
    }
}