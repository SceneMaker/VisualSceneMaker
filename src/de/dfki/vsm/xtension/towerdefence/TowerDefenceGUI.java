package de.dfki.vsm.xtension.towerdefence;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.WindowConstants;
import javax.swing.plaf.basic.BasicInternalFrameUI;

/**
 *
 * @author Jan Stieling
 * 
 */
public class TowerDefenceGUI extends JFrame {

    private Dimension frameSize = new Dimension(900, 600);
    private JInternalFrame iFrame = new JInternalFrame();

    private TowerDefenceExecutor mExecutor;
    private BalloonTowerDefence mBallonTowerDefenceEngine;
    
    TowerDefenceGUI(TowerDefenceExecutor executor) {
        super("Exclusive Tower Defence");
        mExecutor = executor;
        mBallonTowerDefenceEngine  = new BalloonTowerDefence(mExecutor);
        
        mBallonTowerDefenceEngine.setSize(frameSize);
        mBallonTowerDefenceEngine.setMaximumSize(frameSize);

        BasicInternalFrameUI ui = (BasicInternalFrameUI) iFrame.getUI();
        iFrame.putClientProperty("titlePane", ui.getNorthPane());
        iFrame.putClientProperty("border", iFrame.getBorder());
        ui.setNorthPane(null);
        iFrame.setBorder(null);

        iFrame.add(mBallonTowerDefenceEngine); //adds canvas to JInternalFrame
        iFrame.setSize(frameSize.width, frameSize.height);
        iFrame.setPreferredSize(frameSize);
        iFrame.setMaximumSize(frameSize);
        iFrame.setResizable(false);
        iFrame.setVisible(true);
        iFrame.pack();

    //setTitle("Processing Test");
        //setName("Processing Test");
        setLayout(new BorderLayout());

        add(iFrame);
        setMinimumSize(frameSize);
        setPreferredSize(frameSize);
    //pack();

        mBallonTowerDefenceEngine.init();

        // Ding soll zugehen, wenn ich das rote X drücke
        setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);

        setResizable(true);
        while (mBallonTowerDefenceEngine.defaultSize && !mBallonTowerDefenceEngine.finished) {
            try {
                Thread.sleep(5);
            } catch (Exception e) {
            }
        }
        
        setVisible(true);
    }
    
    // Interface Methods
    public void startGame() {
       mBallonTowerDefenceEngine.startSpiel();
    }
        
    protected JComponent makeTextPanel(String text) {
        JPanel panel = new JPanel(false);
        JLabel filler = new JLabel(text);
        filler.setHorizontalAlignment(JLabel.CENTER);
        panel.setLayout(new GridLayout(1, 1));
        panel.add(filler);
        return panel;
    }
}
