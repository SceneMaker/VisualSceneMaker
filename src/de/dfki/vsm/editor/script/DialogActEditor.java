package de.dfki.vsm.editor.script;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.editor.Editor;
import de.dfki.vsm.editor.dialog.DialogActAttributes;
import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.model.project.ProjectData;
import de.dfki.vsm.runtime.dialogact.DialogActInterface;
import de.dfki.vsm.util.evt.EventCaster;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
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
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 * @author Sergio Soto
 */
class DialogActEditor extends JPanel implements EventListener, Observer {
    
    
    private final Observable         mObservable = new DialogActEditor.Observable();
    // Main Pane Containers
    private JSplitPane               mMainSplitPane;
    private JPanel                   mDialogActsPanel;
    private JPanel                   mUtterancePanel;
    // Left Panel
    private JPanel                   mDATitlePanel;
    private JLabel                   mDATitleLabel;
    private JScrollPane              mPhasesListPanel;
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
    private final List<DialogAct> mDialogActList;
    
            
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
        // Init components
        initDialogActsPanel();
        initUtterancePanel();
        // set up Dialog Editor
        mMainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, mDialogActsPanel, mUtterancePanel);
        mMainSplitPane.setResizeWeight(0.2);
        mMainSplitPane.setDividerSize(1);        
        setLayout(new GridLayout(0, 1));  
       
        add(mMainSplitPane);
    }

    private void initDialogActsPanel() {
        
        initTitlePanel();
        loadDialogActs();        
       
        mDialogActsPanel = new JPanel();
        mDialogActsPanel.setLayout(new BoxLayout(mDialogActsPanel, BoxLayout.Y_AXIS));
        
        mDialogActsPanel.add(mDATitlePanel);
        mDialogActsPanel.add(mPhasesListPanel);        
    }
    
    private void initTitlePanel() {
        
        JButton addButton;
        
        addButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/plus.png"));
        addButton.setMaximumSize(new Dimension(20, 20));
        addButton.setPreferredSize(new Dimension(20, 20));
	addButton.setMinimumSize(new Dimension(20, 20));   
        addButton.setBorder(BorderFactory.createEmptyBorder());
        addButton.setOpaque(true);
        addButton.setBackground(Color.GRAY);
 
        
        addButton.addActionListener(new ActionListener() {      
            @Override
            public void actionPerformed(ActionEvent e) {
                
                // Add new phase
              
            }
        });
        
        mDATitleLabel = new JLabel("DialogueActs & Phases");
        mDATitleLabel.setBackground(Color.DARK_GRAY);
        mDATitleLabel.setForeground(Color.WHITE);
        
        mDATitlePanel = new JPanel();
        mDATitlePanel.setLayout(new BoxLayout(mDATitlePanel, BoxLayout.X_AXIS));
        mDATitlePanel.setBackground(Color.LIGHT_GRAY);
        mDATitlePanel.setMaximumSize(new Dimension(20000, 20)); // TODO: change that 2000 to actual width   
        mDATitlePanel.add(Box.createRigidArea(new Dimension(10, 0)));       
        mDATitlePanel.setBackground(Color.DARK_GRAY);
        mDATitlePanel.setForeground(Color.WHITE);
        
        mDATitlePanel.add(mDATitleLabel);        
        mDATitlePanel.add(Box.createHorizontalGlue());
        mDATitlePanel.add(addButton);
    }

    private void loadDialogActs() {
       
        JPanel container = new JPanel();
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        
        mPhasesListPanel = new JScrollPane(container);
        mPhasesListPanel.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_ALWAYS);
                
        // For every phase, add the corresponfing dialogActList
        mDAJLists.clear();
        for (String phaseName : mDialogAct.getDialogueActPhases()) {
             // container contains all phase-dialogActsList pairs     
            container.add(loadPhase(phaseName));
        }        
    }
    
    /**
     *
     */
    private JPanel loadPhase(final String phase) {
        
        JPanel phaseContainer = new JPanel();
        phaseContainer.setLayout(new BoxLayout(phaseContainer, BoxLayout.X_AXIS));           
        
        final JTextField phaseLabel = new JTextField(10);
        phaseLabel.setText("  " + phase);
        phaseLabel.setForeground(Color.black);
        phaseLabel.setBorder(null);
        phaseLabel.setEditable(false);
        phaseLabel.setOpaque(true);
        phaseLabel.setMaximumSize(phaseLabel.getPreferredSize());
        
        phaseLabel.setForeground(Color.black);
        
        phaseLabel.addMouseListener(new MouseAdapter(){
            @Override
            public void mouseClicked(MouseEvent e){
               if (e.getClickCount() == 2) {
                        phaseLabel.setEditable(true);
                             phaseLabel.setBackground(Color.white);
                    
   
               }                      
            }
        });
           
        phaseLabel.addFocusListener(new FocusListener() {
            @Override
            public void focusGained(FocusEvent e) {

            }

            @Override
            public void focusLost(FocusEvent e) {
                phaseLabel.setEditable(false);
                

                if ((mDialogAct.getDialogueActPhases().indexOf(phase) % 2)==0){
                    phaseLabel.setBackground(Color.LIGHT_GRAY); 
                }
                else{
                    phaseLabel.setBackground(UIManager.getColor("TableHeader.background"));            
                }
            }
        });

               
        // Fill list with all dialog acts from this phase
        DefaultListModel dialogActListModel = new DefaultListModel();
        
        for (String dialogAct : mDialogAct.getDialogueActs(phase)) {
            dialogActListModel.addElement(dialogAct);            
        }
        
        final JList dialogActList = new JList(dialogActListModel);
        mDAJLists.add(dialogActList);
                 
        // Dialog Act List properties
        dialogActList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);       
        dialogActList.setSelectionBackground(Color.DARK_GRAY);
        dialogActList.setSelectionForeground(Color.WHITE);
        dialogActList.setOpaque(true);      
        dialogActList.setVisibleRowCount(3);
        dialogActList.setBorder(null);
        dialogActList.setFixedCellHeight(25);
       // dialogActList.setFixedCellWidth(100);
        
        
        JScrollPane listScrollPane = new JScrollPane(dialogActList);          
        listScrollPane.setPreferredSize(new Dimension(10,25*dialogActListModel.getSize()));      
        listScrollPane.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_NEVER);
        listScrollPane.setBorder(null);
        
        phaseLabel.setPreferredSize(new Dimension(110,25*dialogActListModel.getSize())); 
        
        JButton addButton;
        addButton = new JButton(ResourceLoader.loadImageIcon("/res/img/new/plus.png"));
        addButton.setPreferredSize(new Dimension(18,25*dialogActListModel.getSize()));           
        addButton.setMinimumSize(new Dimension(18,25*dialogActListModel.getSize()));      
        addButton.setMaximumSize(new Dimension(18,25*dialogActListModel.getSize()));      
        addButton.setOpaque(true);
        addButton.setBorder(BorderFactory.createEmptyBorder());
        addButton.addActionListener(new ActionListener() {            
            @Override
            public void actionPerformed(ActionEvent e) {                      
                // Add Dialog Act
            }
         });  
        
        
                
        if ((mDialogAct.getDialogueActPhases().indexOf(phase) % 2)==0){
            phaseContainer.setBackground(Color.LIGHT_GRAY);
            dialogActList.setBackground(Color.LIGHT_GRAY); 
            phaseLabel.setBackground(Color.LIGHT_GRAY); 
            addButton.setBackground(Color.LIGHT_GRAY.darker()); 
        }
        else{
            phaseContainer.setBackground(UIManager.getColor("TableHeader.background"));
            dialogActList.setBackground(UIManager.getColor("TableHeader.background"));
            phaseLabel.setBackground(UIManager.getColor("TableHeader.background"));       
            addButton.setBackground(UIManager.getColor("TableHeader.background"));  
            addButton.setBackground(UIManager.getColor("TableHeader.background").darker()); 
        }

        
      //  phasePanel.setMaximumSize(new Dimension(20, 50));
      
        phaseContainer.add(listScrollPane);
        phaseContainer.add(addButton);
        //phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));
        phaseContainer.add(phaseLabel);
        //phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));
        
        // set click listener for text areas
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
                     
                try{
                    mUtteranceTextArea.setText(mDialogAct.getUtterances(dialogActList.getSelectedValue().toString(),mAttributeValueMap));
                    mFMLTextArea.setText(mDialogAct.getFMLCommands(dialogActList.getSelectedValue().toString(),mAttributeValueMap).toString());             
                }catch(Exception ex){}
            }
        });
        
        dialogActList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                JList list = (JList) evt.getSource();                
                
                if (evt.getClickCount() == 2) {                   
                    DialogActAttributes daAttributeDialog = new DialogActAttributes(mDialogAct, dialogActList.getSelectedValue().toString());
                    daAttributeDialog.run();
                }
            }
        });
        
        return phaseContainer;
    }
    
    
    
    private String getAttributeSelectedValue(String atttribute) {
        // TODO
        return "pending";
    }

    private void initUtterancePanel() {
        mUtteranceTitle = new JLabel("Utterance Text/ FML Command");
        mUtteranceTitle.setBackground(Color.DARK_GRAY);
        mUtteranceTitle.setForeground(Color.WHITE);

        // Create Text Button
        mTextButton = new JButton("Text");
        mTextButton.setOpaque(true);
        mTextButton.setBackground(Color.lightGray);
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
        mFMLButton.setOpaque(true);
        mFMLButton.setBackground(Color.lightGray);
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
        mUtteranceTitlePanel.add(Box.createRigidArea(new Dimension(1, 0)));
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