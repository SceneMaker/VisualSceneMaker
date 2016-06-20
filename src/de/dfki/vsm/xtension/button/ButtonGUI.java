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

    final JFXPanel mJFXPanel;

    int mHeight = 700;
    int mWidth = 700;
    float mScale = 1.0f;

    final Group mRootNode;

    SubScene mButtonsSubScene;

    private ButtonGUIExecutor mExecutor;

    public boolean mAlwaysOnTop = true;
    public boolean mHideOnPressed = true;
    public boolean mModal = true;

    private HashMap<String, Button> mButtons;
    private HashMap<String, Point2D> mButtonsPositions;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public ButtonGUI(ButtonGUIExecutor executor) {
        mJFXPanel = new JFXPanel();
        mRootNode = new Group();

        mExecutor = executor;
        //init stuff
        mButtons = new HashMap<>();
        mButtonsPositions = new HashMap<>();

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
            setVisible(!mHideOnPressed);
        });

        mButtons.put(id, b);
        mButtonsPositions.put(id, new Point2D(x, y));
    }

    public void hideAllButtons() {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                for (Button b : mButtons.values()) {
                    //b.setManaged(false);
                    b.setTranslateY(-1000.0); // move the button away
                    b.setVisible(false);
                    //mLogger.message("Hidden Button " + b.getText() + " has coordinates " + b.getTranslateX() + ", " + b.getTranslateY());
                }
            }
        });
    }

    public void showButton(String id, boolean show) {
        Platform.runLater(new Runnable() {
            @Override
            public void run() {
                if (mButtons.containsKey(id)) {
                    mButtons.get(id).setTranslateY(mButtonsPositions.get(id).getY());
                    //mButtons.get(id).setManaged(show);
                    mButtons.get(id).setVisible(show);

                    // mLogger.message("Shown Button " + mButtons.get(id).getText() + " has coordinates " + mButtons.get(id).getTranslateX() + ", " + mButtons.get(id).getTranslateY());
                }
            }
        });
    }

    public void initFX() {
        mButtonsSubScene = new SubScene(mRootNode, mWidth, mHeight, true, SceneAntialiasing.BALANCED);
        mButtonsSubScene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mButtonsSubScene.setBlendMode(BlendMode.MULTIPLY);

        if (mExecutor != null) {
            for (ButtonValues bv : mExecutor.mButtonsAndValues) {
                buildButton(bv.mId, bv.mX, bv.mY, bv.mSize, bv.mName, bv.mValue, bv.mVSMVar);
            }
        }

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
        StackPane rootPane = new StackPane();
        rootPane.getChildren().add(group);
        rootPane.setStyle("-fx-background-color: #FFFFFF00;");

        // build scene
        Scene scene = new Scene(rootPane, mWidth, mHeight);
        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mJFXPanel.setScene(scene);
    }

    public static void main(String[] args) {
        ButtonGUI g = new ButtonGUI(null);
        g.buildButton("b1", 200, 200, 96, "Button", "pressed_button", "var");
        g.initFX();
        g.setVisible(true);
    }
}
