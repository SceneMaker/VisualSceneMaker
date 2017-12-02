/*
 * Add search function in scene
 * 
 */
package de.dfki.vsm.editor.project.auxiliary.scenescript;

import de.dfki.vsm.editor.util.sceneScript.InsideScriptFinder;
import de.dfki.vsm.editor.util.sceneScript.MatchFinder;
import de.dfki.vsm.editor.util.sceneScript.SceneFinder;
import de.dfki.vsm.editor.util.sceneScript.document.DocumentHighlighter;
import de.dfki.vsm.editor.util.sceneScript.document.beans.HighlightInformation;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JTextField;
import javax.swing.text.BadLocationException;

/**
 *
 * @author Robbie
 */
public class SearchFunctionInScene {

    private HighlightInformation highlightInformation;
    private final ScriptEditorPane editorPane;
    Box vSearchBox;
    Box hSearchBox1;
    JTextField textField_FindInput;
    JButton button_Find;
    JButton button_FindPrev;
    JButton button_FindNext;
    JCheckBox checkBox_SearchControl;
    Boolean search_Scene_Name_Symbol = false;

    Box hSearchBox2;
    JTextField textField_ReplaceInput;
    JButton button_Replace;
    JButton button_ReplaceAll;
    JButton button_EmptytextField;

    JButton button_appearanceControl;
    private DocumentHighlighter sceneHighlighter;

    public SearchFunctionInScene(ScriptEditorPane mEditorPane) {
        this.editorPane = mEditorPane;


    }

    public void createSearchBox(OLDSceneScriptEditor SceneScriptEditor) {
        vSearchBox = Box.createVerticalBox();

        // first level search function
        hSearchBox1 = Box.createHorizontalBox();
        hSearchBox1.setBorder(BorderFactory.createEmptyBorder(0, 300, 0, 0));
        button_appearanceControl = new JButton("Find...");
        //button_appearanceControl.setBorder(BorderFactory.createEmptyBorder(0, 200, 0, 0));
        textField_FindInput = new JTextField("", 15);
        textField_FindInput.setMaximumSize(new Dimension(90, 30));
        button_Find = new JButton("Find");
        button_FindPrev = new JButton("<");
        button_FindNext = new JButton(">");
        checkBox_SearchControl = new JCheckBox("Search Only Scenegroup Name");
        checkBox_SearchControl.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                sceneHighlighter = null;
            }
        });
        //hSearchBox1.add(button_appearanceControl);
        hSearchBox1.add(textField_FindInput);
        hSearchBox1.add(button_Find);
        hSearchBox1.add(button_FindPrev);
        hSearchBox1.add(button_FindNext);
       // hSearchBox1.add(checkBox_SearchControl);
        //hSearchBox1.add(button_appearanceControl);
        hSearchBox1.add(Box.createHorizontalGlue());

        // second level replace function
        hSearchBox2 = Box.createHorizontalBox();
        hSearchBox2.setBorder(BorderFactory.createEmptyBorder(0, 375, 0, 0));
        textField_ReplaceInput = new JTextField("", 15);
        textField_ReplaceInput.setMaximumSize(new Dimension(90, 30));
        button_Replace = new JButton("Replace");
        button_ReplaceAll = new JButton("Replace All");
        button_EmptytextField = new JButton("Reset");
        hSearchBox2.add(textField_ReplaceInput);
        hSearchBox2.add(button_Replace);
        hSearchBox2.add(button_ReplaceAll);
        hSearchBox2.add(button_EmptytextField);
        hSearchBox2.add(Box.createHorizontalGlue());

        vSearchBox.add(hSearchBox1);
        //vSearchBox.add(hSearchBox2);
        vSearchBox.add(Box.createVerticalGlue());
        SceneScriptEditor.add(vSearchBox, BorderLayout.NORTH);
        hideSearchBox();
                
        add_listener();

    }

    private void add_listener() {
        
        button_appearanceControl.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if(button_appearanceControl.getText()=="Find..."){
                    button_appearanceControl.setText("Hide");
                    showSearchBox();
                }else{
                    button_appearanceControl.setText("Find...");
                    hideSearchBox();
                }
            }
        });
                
        button_Find.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                find(textField_FindInput.getText());
            }
        });

        button_FindPrev.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                findPrev(textField_FindInput.getText());
            }
        });

        button_FindNext.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                findNext(textField_FindInput.getText());
            }
        });

        button_Replace.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                replace(textField_FindInput.getText(), textField_ReplaceInput.getText());
            }
        });

        button_ReplaceAll.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                replaceAll(textField_FindInput.getText(), textField_ReplaceInput.getText());
            }
        });

        checkBox_SearchControl.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == 1) {
                    search_Scene_Name_Symbol = true;
                } else {
                    search_Scene_Name_Symbol = false;
                }
            }
        });

        button_EmptytextField.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                emptytextField();
            }
        });
    }
    
    private void emptytextField() {
        textField_FindInput.setText("");
        textField_ReplaceInput.setText("");
    }
    
    public void hideSearchBox() {
        textField_FindInput.setVisible(false);
        button_Find.setVisible(false);
        button_FindPrev.setVisible(false);
        button_FindNext.setVisible(false);
        checkBox_SearchControl.setVisible(false);

        textField_ReplaceInput.setVisible(false);
        button_Replace.setVisible(false);
        button_ReplaceAll.setVisible(false);
        button_EmptytextField.setVisible(false);
    }

    public void showSearchBox() {
        textField_FindInput.setVisible(true);
        button_Find.setVisible(true);
        button_FindPrev.setVisible(true);
        button_FindNext.setVisible(true);
        checkBox_SearchControl.setVisible(true);

        textField_ReplaceInput.setVisible(true);
        button_Replace.setVisible(true);
        button_ReplaceAll.setVisible(true);
        button_EmptytextField.setVisible(true);
    }

    public void find(String findInput) {
        try {
            getScriptHighlighter(findInput).all();
        } catch (BadLocationException e) {
            e.printStackTrace();
        }

    }

    private void findPrev(String findInput) {
        try {
            getScriptHighlighter(findInput).previous();
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }

    private DocumentHighlighter getScriptHighlighter(String findInput){
        if(sceneHighlighter == null){
            sceneHighlighter = createSceneHighlighter(findInput);
        }
        highlightInformation.setWordToFind(findInput);
        return  sceneHighlighter;
    }

    private void findNext(String findInput) {
        try {
            getScriptHighlighter(findInput).next();
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }

    private DocumentHighlighter createSceneHighlighter(String findInput) {
         highlightInformation = new HighlightInformation(
                editorPane.getDocument(),
                findInput,
                editorPane.getHighlighter(),
                editorPane
        );
        MatchFinder finder = new InsideScriptFinder(highlightInformation);
        if(checkBox_SearchControl.isSelected()){
             finder = new SceneFinder(highlightInformation);
         }


        return new DocumentHighlighter(highlightInformation, finder);
    }

    public void replace(String findInput, String replaceInput) {

    }

    public void replaceAll(String findInput, String replaceInput) {

    }
}
