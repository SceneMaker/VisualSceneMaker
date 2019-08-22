/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.button;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.geometry.Pos;
import javafx.geometry.Rectangle2D;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.SceneAntialiasing;
import javafx.scene.SubScene;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.effect.BlendMode;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.stage.Modality;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class GUIRenderer extends Application {

    public boolean mHideOnPressed = true;
    public boolean mAlwaysOnTop = true;
    public boolean mModal = true;

    private ButtonGUIExecutor mExecutor = null;

    private Stage mStage;

    private static boolean mIsRunning = false;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public static boolean isRunning() {
        return mIsRunning;
    }

    public void setButtonExecutor(ButtonGUIExecutor be) {
        mExecutor = be;
    }

    @Override
    public void start(Stage primaryStage) {

        Platform.setImplicitExit(false);

        StackPane sp = new StackPane();
        sp.setStyle("-fx-background-color: #FFFFFF00;");
        Scene scene = new Scene(sp, 1, 1);

        scene.setFill(javafx.scene.paint.Color.TRANSPARENT);

        primaryStage.initStyle(StageStyle.TRANSPARENT);
        primaryStage.setScene(scene);
        primaryStage.show();
        // hide it since we dont need it ...
        primaryStage.hide();

        mIsRunning = true;
    }

    public void create() {
        if (!mIsRunning) {
            launch();
        }
    }

    public void showGUIElement(String[] elements) {
        mStage = new Stage();

        Screen screen = Screen.getPrimary();
        Rectangle2D bounds = screen.getVisualBounds();

        Group buttonNode = new Group();
        SubScene buttonsSubScene = new SubScene(buttonNode, bounds.getWidth(), bounds.getHeight(), true, SceneAntialiasing.BALANCED);
        buttonsSubScene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        buttonsSubScene.setBlendMode(BlendMode.MULTIPLY);

        mStage.setX(bounds.getMinX());
        mStage.setY(bounds.getMinY());
        mStage.setWidth(bounds.getWidth());
        mStage.setHeight(bounds.getHeight());

        mStage.initStyle(StageStyle.TRANSPARENT);

        mStage.initModality((mModal) ? Modality.APPLICATION_MODAL : Modality.NONE);
        mStage.setAlwaysOnTop(mAlwaysOnTop);

        // build gui elements
        for (String e : elements) {
            e = e.trim(); // get rid of some white space that might be there.

            GUIElementValues bv = mExecutor.mButtonsAndValues.get(e);

            if (bv.mId.contains("button")) {
                Button button = new Button();
                button.setText(bv.mName);
                button.setFont(Font.font(Font.getDefault().getName(), bv.mSize));
                button.setTranslateX(bv.mX);
                button.setTranslateY(bv.mY);
                button.setOnAction(arg0 -> {
                    // set SceneMaker variable
                    if (mExecutor != null) {
                        mExecutor.setVSmVar(bv.mVSMVar, bv.mValue);
                    }
                    // hide only if option hide on pressed is set true
                    if (mHideOnPressed) {
                        mStage.close();
                        mStage = null;
                    }
                });

                buttonNode.getChildren().add(button);
            }

            if (bv.mId.contains("textfield")) {
                TextField textfield = new TextField();
                textfield.setText(bv.mValue);
                textfield.setFont(Font.font(Font.getDefault().getName(), bv.mSize));
                textfield.setTranslateX(bv.mX);
                textfield.setTranslateY(bv.mY);
                textfield.setOnAction(arg0 -> {
                    // set SceneMaker variable
                    if (mExecutor != null) {
                        mExecutor.setVSmVar(bv.mVSMVar, textfield.getText());
                    }
                    // hide only if option hide on pressed is set true
                    if (mHideOnPressed) {
                        mStage.close();
                        mStage = null;
                    }
                });

                buttonNode.getChildren().add(textfield);
            }
        }

        // build layout
        StackPane sp = new StackPane();
        StackPane.setAlignment(buttonsSubScene, Pos.TOP_CENTER);
        sp.getChildren().add(buttonsSubScene);
        sp.setStyle("-fx-background-color: #FFFFFF00;");

        //scaling
        Group group = new Group(sp);
        group.setScaleX(group.getScaleX() * 1);
        group.setScaleY(group.getScaleY() * 1);

        // place centered
        StackPane rp = new StackPane();
        rp.getChildren().add(group);
        rp.setStyle("-fx-background-color: #FFFFFF00;");

        Scene scene = new Scene(rp);
        
        if (mModal) {
            scene.setFill(new Color(1.0, 1.0, 1.0, 0.004));
        } else {
            scene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        }
        mStage.setScene(scene);
        mStage.show();
    }

    public void hideButton() {
        if (mStage != null) {
            mStage.close();
            mStage = null;
        }
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }

}
