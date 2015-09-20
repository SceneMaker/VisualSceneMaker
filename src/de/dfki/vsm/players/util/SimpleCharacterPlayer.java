package de.dfki.vsm.players.util;
 
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import java.awt.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import javax.swing.*;

 
public final class SimpleCharacterPlayer extends JFrame {
    
    private static final SimpleCharacterPlayer instance    = null;
   
    private final int mHeight = 500;
    private final int mWidth = 800;
    
    private final Color mForegroundColor = new Color(188, 188, 188);
    private final Color mTextBackgroundColor = new Color(49, 49, 49);
    
    private final JPanel mMainPanel = new JPanel();
    private final JPanel mUpperPanel = new JPanel();
    private final JPanel mBottomPanel = new JPanel();
    
    private final JTextArea mTextArea = new JTextArea();
        
    //private final ArrayList<Stickman> mCharacterList = new ArrayList<>();
    private final HashMap<String, Stickman> mCharacterList = new HashMap<>();
    
    private final Set<String> mCharacterSet;
   
    
    public SimpleCharacterPlayer(SceneScript scenescript) {
       
        JFrame frame = new JFrame("Simple Character Player");
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        // Add existing characters
        mCharacterSet = getCharacters(scenescript);
        
        for(String chracterName: mCharacterSet ){
            mCharacterList.put(chracterName,new Stickman(chracterName, 200,350));
        }
        
        initPanel();

        frame.add(mMainPanel);
        frame.setSize(mWidth, mHeight);
        frame.setVisible(true);
    }
    
    private void initPanel(){
        mMainPanel.setLayout(new BoxLayout(mMainPanel, BoxLayout.Y_AXIS)); 
  
        
        mUpperPanel.setLayout(new BoxLayout(mUpperPanel, BoxLayout.X_AXIS));
        mUpperPanel.setBackground(mTextBackgroundColor);  
       
        mBottomPanel.setLayout(new BoxLayout(mBottomPanel, BoxLayout.X_AXIS));
        mBottomPanel.setBackground(mTextBackgroundColor);   
        mBottomPanel.setMinimumSize(new Dimension(mWidth, 70));
        mBottomPanel.setPreferredSize(new Dimension(mWidth, 70));
        mBottomPanel.setMaximumSize(new Dimension(mWidth, 70));

        mTextArea.setBackground(mTextBackgroundColor);
        mTextArea.setForeground(mForegroundColor);
        mTextArea.setMinimumSize(new Dimension(mWidth, 70));
        mTextArea.setPreferredSize(new Dimension(mWidth, 70));
        mTextArea.setMaximumSize(new Dimension(mWidth, 70));
        
        mBottomPanel.add(Box.createRigidArea(new Dimension(20,0)));
        mBottomPanel.add(mTextArea);
        
        for(Stickman ch: mCharacterList.values()){
            mUpperPanel.add(ch);  
        }
        
        
        mMainPanel.add(mUpperPanel);
        mMainPanel.add(mBottomPanel);
    }
    
    public void launch(){
        
        this.setVisible(true);
    }
    
    public void performAction(String character, ActionObject action){
        
        String actionString = ((ActionObject) action).getText();
        // Here we have to take into account intensity, so we will receive
        // something like [happy 0.3], we need to find a way to parse this
        // and send it to Stickman, What about functions taking a parameter (double)?
        // if no intensity is detected we go for 0.5, do you agree?
        
        // Now we also need to now which character invokes the action
        // fot testing purposes I will assume the first character always
     
        double intensity = 1.0;
        
        switch(actionString){
            case "[happy]":
                mCharacterList.get(character).happy(intensity);
                break;
            case "[sad]":
                mCharacterList.get(character).sad(intensity);
                break;
            case "[fear]":
                mCharacterList.get(character).scared(intensity);
                break;
            case "[angry]":
                mCharacterList.get(character).angry(intensity);
                break;
            case "[disgussed]":
                mCharacterList.get(character).disgussed(intensity);
                break;
            case "[shame]":
                mCharacterList.get(character).shame(intensity);
                break;
            case "[box]":
                mCharacterList.get(character).box();
                break;
            case "[wave]":
                mCharacterList.get(character).wave();
                break;
            case "[cup]":
                mCharacterList.get(character).cup();
                break;
            case "[scratch]":
                mCharacterList.get(character).scratch();
                break;
            default:
                break;
        }
    }
    
    public void speak(String character,String msg){
        mCharacterList.get(character).speak(msg);
    }
	
    public void displayText(String text){
        mTextArea.setText("\n"+text);
    }
    
    public Set<String> getCharacters(SceneScript scenescript){

        Set<String> speakersSet = new HashSet<>();
        
        for(SceneObject scene: scenescript.getSceneList()){
            // Old: speakersSet.add(scene.getTurnList().getFirst().getSpeaker());
			// PG replaced by this to catch _all_ speakers
			LinkedList<SceneTurn> sturns = scene.getTurnList();
			for (SceneTurn t : sturns) {
				if (!speakersSet.contains(t.getSpeaker())) {
					speakersSet.add(t.getSpeaker());
				}
			}
        }
        
        //System.out.println("there are " + speakersSet.size()+ " characters");
        
        return speakersSet;
    }
}