package de.dfki.vsm.editor.project.auxiliary.dialogact;

//~--- non-JDK imports --------------------------------------------------------
import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.AddButton;
import de.dfki.vsm.editor.dialog.DialogActAttributes;
import de.dfki.vsm.editor.event.DialogActSelectedEvent;
import de.dfki.vsm.model.dialogact.DialogAct;
import de.dfki.vsm.runtime.dialogacts.DialogActInterface;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.ios.ResourceLoader;

//~--- JDK imports ------------------------------------------------------------
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.ImageIcon;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.UIManager;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS;
import static javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER;

/**
 * @author Sergio Soto
 */
public final class DialogActEditor extends JPanel implements EventListener {

    // private final Observable mObservable = new DialogActEditor.Observable();
    private Dimension buttonSize = new Dimension(125, 30);
    ;

    // Main Pane Containers
    private JSplitPane mMainSplitPane;
    private JPanel mDialogActsPanel;
    private JPanel mUtterancePanel;

    // Left Panel
    private JPanel mDATitlePanel;
    private JLabel mDATitleLabel;
    private JScrollPane mPhasesListPanel;

    // Right Panel
    private JPanel mUtteranceTitlePanel;
    private JLabel mUtteranceTitle;
    private JLabel mTextButton;
    private JLabel mFMLButton;
    private JScrollPane mUtteranceTextPanel;
    private JTextArea mUtteranceTextArea;
    private JTextArea mFMLTextArea;

    //
    private final EditorProject mProject;
    private final DialogActInterface mDialogAct;
    private final List<JList> mDAJLists;
    private final List<DialogAct> mDialogActList;
    
    //ICONS 
    private final ImageIcon ICON_TEXT_STANDARD = ResourceLoader.loadImageIcon("/res/img/text_icon_gray.png");
    private final ImageIcon ICON_TEXT_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/text_icon_gray.png");
    
    private final ImageIcon ICON_FML_STANDARD = ResourceLoader.loadImageIcon("/res/img/fml_gray.png");
    private final ImageIcon ICON_FML_ROLLOVER = ResourceLoader.loadImageIcon("/res/img/fml_blue.png");
    //
    public final void close() {
        // Remove All Observers
        //mObservable.deleteObservers();
        // TODO: Close the dialog act editor
    }

    public DialogActEditor(EditorProject project) {
        mProject = project;
        mDialogAct = mProject.getDialogAct();
        setBackground(Color.white);
        setMinimumSize(new Dimension(0, 200));
        mDAJLists = new ArrayList<>();
        mDialogActList = new ArrayList<>();
        initComponents();

        // Add the element editor to the event multicaster
        EventDispatcher.getInstance().register(this);
    }

    private void initComponents() {

        // Init components
        initDialogActsPanel();
        initUtterancePanel();

        // set up Dialog Editor
        mMainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, mDialogActsPanel, mUtterancePanel);
        mMainSplitPane.setOpaque(false);
        mMainSplitPane.setResizeWeight(0.2);
        mMainSplitPane.setDividerSize(1);
        setLayout(new GridLayout(0, 1));
        add(mMainSplitPane);
    }

    private void initDialogActsPanel() {
        initTitlePanel();
        loadDialogActs();
        mDialogActsPanel = new JPanel();
        mDialogActsPanel.setOpaque(false);
        mDialogActsPanel.setLayout(new BoxLayout(mDialogActsPanel, BoxLayout.Y_AXIS));
        mDialogActsPanel.add(mDATitlePanel);
        mDialogActsPanel.add(mPhasesListPanel);
    }

    private void initTitlePanel() {
        AddButton addButton;

        addButton = new AddButton();
        addButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {

                // Add new phase
            }
        });
        mDATitleLabel = new JLabel("DialogueActs & Phases");
        mDATitleLabel.setBackground(Color.DARK_GRAY);
        mDATitleLabel.setForeground(Color.WHITE);
        mDATitlePanel = new JPanel();
        mDATitlePanel.setLayout(new BoxLayout(mDATitlePanel, BoxLayout.X_AXIS));
        mDATitlePanel.setMaximumSize(new Dimension(20000, 20));    // TODO: change that 2000 to actual width
        mDATitlePanel.add(Box.createRigidArea(new Dimension(10, 0)));
        mDATitlePanel.setBackground(Color.DARK_GRAY);
        mDATitlePanel.setForeground(Color.WHITE);
        mDATitlePanel.add(mDATitleLabel);
        mDATitlePanel.add(Box.createHorizontalGlue());
        //mDATitlePanel.add(addButton);
    }

    private void loadDialogActs() {
        JPanel container = new JPanel();

        container.setOpaque(false);
        container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));
        mPhasesListPanel = new JScrollPane(container);
        mPhasesListPanel.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        mPhasesListPanel.setOpaque(false);
        mPhasesListPanel.getViewport().setOpaque(false);
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

        phaseContainer.setOpaque(false);
        phaseContainer.setLayout(new BoxLayout(phaseContainer, BoxLayout.X_AXIS));
        phaseContainer.setBorder(BorderFactory.createRaisedBevelBorder());

        final JTextField phaseLabel = new JTextField(10);

        phaseLabel.setText("  " + phase);
        phaseLabel.setForeground(Color.black);
        phaseLabel.setBorder(null);
        phaseLabel.setEditable(false);
        phaseLabel.setOpaque(true);
        phaseLabel.setMaximumSize(phaseLabel.getPreferredSize());
        phaseLabel.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
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

                if ((mDialogAct.getDialogueActPhases().indexOf(phase) % 2) == 0) {
                    phaseLabel.setBackground(Color.LIGHT_GRAY);
                } else {
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

        listScrollPane.setOpaque(false);
        listScrollPane.getViewport().setOpaque(false);
        listScrollPane.setPreferredSize(new Dimension(10, 25 * dialogActListModel.getSize()));
        listScrollPane.setVerticalScrollBarPolicy(VERTICAL_SCROLLBAR_NEVER);
        listScrollPane.setBorder(null);
        phaseLabel.setPreferredSize(new Dimension(110, 25 * dialogActListModel.getSize()));

        AddButton addButton;

        addButton = new AddButton();
        addButton.setBackground(Color.white);
        addButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {

                // Add Dialog Act
            }
        });

//      if ((mDialogAct.getDialogueActPhases().indexOf(phase) % 2)==0){
//          phaseContainer.setBackground(Color.LIGHT_GRAY);
//          dialogActList.setBackground(Color.LIGHT_GRAY); 
//          phaseLabel.setBackground(Color.LIGHT_GRAY); 
//          addButton.setBackground(Color.LIGHT_GRAY.darker()); 
//      }
//      else{
//          phaseContainer.setBackground(UIManager.getColor("TableHeader.background"));
//          dialogActList.setBackground(UIManager.getColor("TableHeader.background"));
//          phaseLabel.setBackground(UIManager.getColor("TableHeader.background"));       
//          addButton.setBackground(UIManager.getColor("TableHeader.background"));  
//          addButton.setBackground(UIManager.getColor("TableHeader.background").darker()); 
//      }
        // phasePanel.setMaximumSize(new Dimension(20, 50));
        phaseContainer.add(listScrollPane);
        phaseContainer.add(addButton);

        // phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));
        phaseContainer.add(phaseLabel);

        // phaseContainer.add(Box.createRigidArea(new Dimension(10, 0)));
        // set click listener for text areas
        dialogActList.addListSelectionListener(new ListSelectionListener() {
            @Override
            public void valueChanged(ListSelectionEvent e) {
                for (JList i : mDAJLists) {
                    if (!(i.equals(e.getSource()))) {
                        i.clearSelection();
                    }
                }

                Map<String, String> mAttributeValueMap = new HashMap();

                for (String atttribute : mDialogAct.getNLGAttributes()) {
                    mAttributeValueMap.put(atttribute, getAttributeSelectedValue(atttribute));
                }

                try {
                    mUtteranceTextArea.setText(mDialogAct.getUtterances(dialogActList.getSelectedValue().toString(),
                            mAttributeValueMap));
                    mFMLTextArea.setText(mDialogAct.getFMLCommands(dialogActList.getSelectedValue().toString(),
                            mAttributeValueMap).toString());
                } catch (Exception ex) {
                }
            }
        });
        dialogActList.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent evt) {
                JList list = (JList) evt.getSource();

                if (evt.getClickCount() == 2) {
                    DialogActAttributes daAttributeDialog = new DialogActAttributes(mDialogAct,
                            dialogActList.getSelectedValue().toString());

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
        mTextButton = new JLabel("Text");
        mTextButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mTextButton.setOpaque(true);
        mTextButton.setBackground(Color.white);
        mTextButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mTextButton.setToolTipText("Uniform");
        mTextButton.setIcon(ICON_TEXT_STANDARD);
        mTextButton.setIconTextGap(20);
        mTextButton.setFont(new Font("Helvetica", Font.PLAIN, 15));
        mTextButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mTextButton.setPreferredSize(buttonSize);
        mTextButton.setMinimumSize(buttonSize);
        mTextButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {

                // Do something
                mFMLTextArea.setVisible(false);
                mUtteranceTextArea.setVisible(true);
            }

            public void mouseEntered(MouseEvent me) {
                mTextButton.setIcon(ICON_TEXT_ROLLOVER);
                mTextButton.setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                mTextButton.setIcon(ICON_TEXT_STANDARD);
                mTextButton.setBackground(new Color(255, 255, 255));
            }
        });

        // Create FML Button
        mFMLButton = new JLabel("FML");
        mFMLButton.setHorizontalAlignment(SwingConstants.RIGHT);
        mFMLButton.setOpaque(true);
        mFMLButton.setBackground(Color.white);
        mFMLButton.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        mFMLButton.setToolTipText("FML Markup Language");
        mFMLButton.setIcon(ICON_FML_STANDARD);
        mFMLButton.setIconTextGap(20);
        mFMLButton.setFont(new Font("Helvetica", Font.PLAIN, 15));
        mFMLButton.setBorder(BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1));
        mFMLButton.setPreferredSize(buttonSize);
        mFMLButton.setMinimumSize(buttonSize);
        mFMLButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                mFMLTextArea.setVisible(true);
                mUtteranceTextArea.setVisible(false);

                // Do something
            }

            public void mouseEntered(MouseEvent me) {
                mFMLButton.setIcon(ICON_FML_ROLLOVER);
                mFMLButton.setBackground(new Color(82, 127, 255));
            }

            public void mouseExited(MouseEvent me) {
                mFMLButton.setIcon(ICON_FML_STANDARD);
                mFMLButton.setBackground(new Color(255, 255, 255));
            }
        });
        mUtteranceTitlePanel = new JPanel();
        mUtteranceTitlePanel.setBackground(Color.DARK_GRAY);
        mUtteranceTitlePanel.setLayout(new BoxLayout(mUtteranceTitlePanel, BoxLayout.X_AXIS));
        mUtteranceTitlePanel.setMaximumSize(new Dimension(20000, 30));    // TODO: change that 2000 to actual width
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
    public void update(EventObject event) {
        if (event instanceof DialogActSelectedEvent) {
            DialogAct dialogActData = ((DialogActSelectedEvent) event).getFunction();

            for (int i = 0; i < mDAJLists.size(); i++) {
                mDAJLists.get(i).clearSelection();
            }

            for (int i = 0; i < mDAJLists.size(); i++) {
                for (int j = 0; j < mDAJLists.get(i).getModel().getSize(); j++) {
                    if (dialogActData.getName().equals(mDAJLists.get(i).getModel().getElementAt(j).toString())) {
                        mDAJLists.get(i).setSelectedIndex(j);
                        mPhasesListPanel.getVerticalScrollBar().setValue(i * 30);

                        break;
                    }
                }
            }
        }
    }

    // Refresh the visual appearance
    public final void refresh() {

    }
}
