/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.buttongui;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.awt.Color;
import java.awt.Dimension;
import java.util.HashMap;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
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
 */
public class Gui extends JFrame {

    final JFXPanel mJFXPanel;

    int mHeight = 700;
    int mWidth = 700;
    float mScale = 1.0f;

    final Group mRootNode;

    SubScene mButtonsSubScene;

    private ButtonGuiExecutor mExecutor;

    private HashMap<String, Button> mButtons;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public Gui(ButtonGuiExecutor executor) {
        mJFXPanel = new JFXPanel();
        mRootNode = new Group();

        mExecutor = executor;
        mButtons = new HashMap<>();

        Dimension size = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
        mWidth = size.width;
        mHeight = size.height;

        super.setName("Button GUI");
        JPanel p = new JPanel();
        p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
        p.setBackground(new Color(0, 0, 0, 0));
        add(p);
        mJFXPanel.setMinimumSize(new Dimension(mWidth, mHeight));
        mJFXPanel.setPreferredSize(new Dimension(mWidth, mHeight));
        p.add(mJFXPanel);
        // Set Not Rezizable
        //mFrame.setResizable(false);
        // Set Always On Top
        setAlwaysOnTop(true);
        // Set Undecorated
        setUndecorated(true);
        // Set Transparent
        setBackground(new Color(0, 0, 0, 1));

        pack();

        //mFrame.setSize(mWidth, mHeight);
        //setExtendedState(JFrame.MAXIMIZED_BOTH);
        setLocationRelativeTo(null);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
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
            System.out.println("user pressed ... " + name);
            mExecutor.setVSmVar(var, value);
        });

        mButtons.put(id, b);
    }

    public void hideAllButtons() {
        for (Button b : mButtons.values()) {
            mLogger.message("hide Button " + b.getText());
            b.setVisible(false);
        }
    }

    public void showButton(String id, boolean show) {
       if (mButtons.containsKey(id)) {
            mButtons.get(id).setVisible(show);
        }
    }

    public void initFX() {
        mButtonsSubScene = new SubScene(mRootNode, mWidth, mHeight, true, SceneAntialiasing.BALANCED);
        mButtonsSubScene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        mButtonsSubScene.setBlendMode(BlendMode.MULTIPLY);

        //buildButton("yes", 200, 200, 48, "Ja", "pressed_yes");
        for (ButtonValues bv : mExecutor.mButtonsAndValues) {
            buildButton(bv.mId, bv.mX, bv.mY, bv.mSize, bv.mName, bv.mValue, bv.mVSMVar);
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
        Gui g = new Gui(null);

        g.setVisible(true);
    }
}
