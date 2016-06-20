/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.button;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.util.HashMap;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.geometry.Point2D;
import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.SceneAntialiasing;
import javafx.scene.SubScene;
import javafx.scene.control.Button;
import javafx.scene.effect.BlendMode;
import javafx.scene.layout.StackPane;
import javafx.scene.text.Font;
import javax.swing.BoxLayout;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ButtonGUI extends JFrame {

    private JFXPanel mJFXPanel;

    int mHeight = 700;
    int mWidth = 700;
    float mScale = 1.0f;

    Group mRootNode;
    SubScene mButtonsSubScene;

    private ButtonGUIExecutor mExecutor;

    private boolean mInitialized = false;
    private boolean mButtonsHidden = false;

    public boolean mAlwaysOnTop = true;
    public boolean mHideOnPressed = true;
    public boolean mModal = true;

    private HashMap<String, Button> mButtons;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ButtonGUI(ButtonGUIExecutor executor) {
        mJFXPanel = new JFXPanel();

        mExecutor = executor;
        //init stuff
        mButtons = new HashMap<>();
        Dimension size = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        mWidth = size.width;
        mHeight = size.height;

        super.setName("Button GUI");
        JPanel p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
        p.setBackground(new Color(0, 0, 0, 0));
        add(p);
        // configure size of jfx 
        mJFXPanel.setMinimumSize(new Dimension(mWidth, mHeight));
        mJFXPanel.setPreferredSize(new Dimension(mWidth, mHeight));
        p.add(mJFXPanel);
        // Set Not Rezizable
        //setResizable(false);
        // Set Always On Top
        setAlwaysOnTop(mAlwaysOnTop);
        // Set Undecorated
        setUndecorated(true);
        // Set Transparent
        setBackground(new Color(0, 0, 0, (mModal) ? 1 : 0));
        // tie everything together
        pack();

        // place it in the middle of the screen
        setLocationRelativeTo(null);
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
    }

    private void buildButton(String id, int x, int y, int size, String name, String value, String var) {
        Button b = new Button();
        b.setText(name);
        b.setFont(Font.font(Font.getDefault().getName(), size));
        b.setTranslateX(x);
        b.setTranslateY(y);

        mRootNode.getChildren().addAll(b);

        // add controller stuff
        b.setOnAction((event) -> {
            // set SceneMaker variable
            if (mExecutor != null) {
                mExecutor.setVSmVar(var, value);
            }
            // hide gui if wanted
            if (mHideOnPressed) {
                hideAllButtons();
                //setVisible(!mHideOnPressed);
            }
        });

        mButtons.put(id, b);
    }

    public void hideAllButtons() {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                for (Button b : mButtons.values()) {
                    double origY = b.getTranslateY();
                    if (origY > 0.0) { // only hide button if it is not hidden!
                        mLogger.message("Hide Button " + b.getText() + " @ " + b.getTranslateY());
                        b.setTranslateY(origY - 2000);
                    }
//                    b.setVisible(false);
//                    b.setManaged(false);
//                    mRootNode.getChildren().remove(b);
                }

                mButtonsHidden = true;

                setVisible(false);
            }
        });
    }

    public void showButton(String id, boolean show) {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {

                while (!mButtonsHidden) {
                    mLogger.message("Waiting until buttons are hidden ...");
                }
                if (mButtons.containsKey(id)) {

                    double origY = mButtons.get(id).getTranslateY();
                    mButtons.get(id).setTranslateY(origY + 2000);

                    mLogger.message("Show Button " + mButtons.get(id).getText() + " @ " + mButtons.get(id).getTranslateY());
//                    mRootNode.getChildren().add(mButtons.get(id));
//                    mButtons.get(id).setManaged(show);
//                    mButtons.get(id).setVisible(show);
                }
                setVisible(true);
            }
        });
    }

    public boolean isInitialized() {
        return mInitialized;
    }

    public void initFX() {
        mLogger.message("Init Button GUI ...");
        mRootNode = new Group();
        mButtonsSubScene = new SubScene(mRootNode, mWidth, mHeight, true, SceneAntialiasing.BALANCED);
        mButtonsSubScene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mButtonsSubScene.setBlendMode(BlendMode.MULTIPLY);

        if (mExecutor != null) {
            for (ButtonValues bv : mExecutor.mButtonsAndValues) {
                buildButton(bv.mId, bv.mX, bv.mY, bv.mSize, bv.mName, bv.mValue, bv.mVSMVar);
            }
        }

        // default hide all
        hideAllButtons();

        // build layout
        StackPane sp = new StackPane();
        StackPane.setAlignment(mButtonsSubScene, Pos.TOP_CENTER);
        sp.getChildren().add(mButtonsSubScene);
        sp.setStyle("-fx-background-color: #FFFFFF00;");

        //scaling
        Group group = new Group(sp);
        group.setScaleX(group.getScaleX() * 1);
        group.setScaleY(group.getScaleY() * 1);

        // place centered
        StackPane rp = new StackPane();
        rp.getChildren().add(group);
        rp.setStyle("-fx-background-color: #FFFFFF00;");

        // build scene
        Scene scene = new Scene(rp, mWidth, mHeight);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mJFXPanel.setScene(scene);
        mInitialized = true;
    }

    public static void main(String[] args) {
        ButtonGUI g = new ButtonGUI(null);
        g.buildButton("b1", 200, 200, 96, "Button", "pressed_button", "var");
        g.initFX();
        g.setVisible(true);
    }
}
