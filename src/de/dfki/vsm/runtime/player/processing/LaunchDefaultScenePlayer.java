package de.dfki.vsm.runtime.player.processing;
 
/**
 * @author Sergio Soto
 *
 */
public class LaunchDefaultScenePlayer extends javax.swing.JFrame {
    
    private static LaunchDefaultScenePlayer instance    = null;
    
    protected LaunchDefaultScenePlayer() {
        this.setSize(300, 410); 
        setTitle("Visual SceneMaker: Default ScenePlayer");    
        javax.swing.JPanel panel = new javax.swing.JPanel();
        this.setResizable(false);        
        processing.core.PApplet sketch = new DefaultScenePlayer();
        panel.add(sketch);
        this.add(panel);
        sketch.init(); //this is the function used to start the execution of the sketch

    }

    public static LaunchDefaultScenePlayer getInstance() {
        if (instance == null) {
            instance = new LaunchDefaultScenePlayer();
        }

        return instance;
    }
    
    public void launch(){
        
        this.setVisible(true);
    }
}



    
 