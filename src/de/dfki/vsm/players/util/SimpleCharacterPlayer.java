package de.dfki.vsm.players.util;
 
import de.dfki.vsm.model.scenescript.ActionFeature;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import java.awt.*;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Set;
import javax.swing.*;

 
public final class SimpleCharacterPlayer extends JFrame {
    
    private static final SimpleCharacterPlayer instance    = null;
   
    private final int mHeight = 500;
    private int mWidth = 500;
    
    private final Color mForegroundColor = new Color(188, 188, 188);
    private final Color mTextBackgroundColor = new Color(49, 49, 49);
    
    private final JPanel mMainPanel = new JPanel();
    private final JPanel mUpperPanel = new JPanel();
    private final JPanel mBottomPanel = new JPanel();
    
    private final JTextArea mTextArea = new JTextArea();
        
    private final LinkedHashMap<String, Stickman> mCharacterList = new LinkedHashMap<>();
    
    private final Set<String> mCharacterSet;
   
    
    public SimpleCharacterPlayer(SceneScript scenescript) {
       
        JFrame frame = new JFrame("Simple Character Player");
        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        // Add existing characters to LinkedHashMap
        mCharacterSet = getCharacters(scenescript);
        for(String chracterName: mCharacterSet ){
            mCharacterList.put(chracterName,new Stickman(chracterName, 200,350));
        }
        
        initPanel();
        frame.add(mMainPanel);
        
        // Width of the Frame depends on the number of characters
        if(mCharacterList.size()> 1){
            mWidth = 250 * mCharacterList.size();
        }
        frame.setSize(mWidth, mHeight);
        frame.setVisible(true);
    }
    
    public void launch(){
        this.setVisible(true);
    }
    
    public void performAction(String characterName, ActionObject action){
        
        action.getName();
        action.getFeatureList();
        
        double intensityValue;
        int lookToPostion;
                
        switch(action.getName()){
            case "happy":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).happy(intensityValue);
                break;
            case "sad":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).sad(intensityValue);
                break;
            case "fear":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).scared(intensityValue);
                break;
            case "angry":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).angry(intensityValue);
                break;
            case "disgussed":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).disgusted(intensityValue);
                break;
            case "shame":
                intensityValue = getIntensityValue(action.getFeatureList());
                mCharacterList.get(characterName).shame(intensityValue);
                break;
            case "box":
                mCharacterList.get(characterName).box();
                break;
            case "wave":
                mCharacterList.get(characterName).wave();
                break;
            case "cup":
                mCharacterList.get(characterName).cup();
                break;
            case "scratch":
                mCharacterList.get(characterName).scratch();
                break;
            case "lookTo":
                lookToPostion = getLookToPosition(characterName, action.getFeatureList());
                mCharacterList.get(characterName).lookTo(lookToPostion);
                break;
            default:
                break;
        }
    }
    
    public void speak(String characterName,String msg){
        mCharacterList.get(characterName).speak(msg);
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
        return speakersSet;
    }
    
    private double getIntensityValue(LinkedList<ActionFeature> attributes){
        
        if(attributes.size()>0){
            if(attributes.getFirst().getKey().equals("intensity")){
                return Double.parseDouble(attributes.getFirst().getVal());
            }
            else{
                return 1.0;
            }
        }
        else{
            return 1.0;
        }
    }
    
    
    private int getLookToPosition(String characterName, LinkedList<ActionFeature> attributes){
    
        if(attributes.size()>0){
            if(attributes.getFirst().getKey().equals("name")){
                
                int objectiveIndex = 0;
                int characterIndex = 0;
                int index = 0;
                
                for (String key : mCharacterList.keySet()) {
                    
                    if(key.equals(characterName)){
                        characterIndex = index;
                    }
                    
                    if(key.equals(attributes.getFirst().getVal())){
                        objectiveIndex = index;
                    }
                    
                    index++;
                }

                if(characterIndex!=objectiveIndex){
                    return (characterIndex>objectiveIndex)?-1:1;
                }
            }
        }
        
        return 0;
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
    
}