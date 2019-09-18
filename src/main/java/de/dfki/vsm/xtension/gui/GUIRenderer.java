/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.gui;

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
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.effect.BlendMode;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.stage.Modality;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

import java.io.File;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class GUIRenderer extends Application {

    boolean mHideOnPressed = true;
    boolean mAlwaysOnTop = true;
    boolean mModal = true;

    private GUIExecutor mExecutor = null;

    private Stage mStage;

    private static boolean mIsRunning = false;

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    static boolean isRunning() {
        return mIsRunning;
    }

    void setButtonExecutor(GUIExecutor buttonExecutor) {
        mExecutor = buttonExecutor;
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

    void create() {
        if (!mIsRunning) {
            launch();
        }
    }

    void showGUIElements(String[] elements) {
        File css = new File(mExecutor.getProjectPath() + File.separator + mExecutor.getProjectConfigVar("css"));
        String cssResource = "file:///" + css.getAbsolutePath();
        cssResource = cssResource.replace("\\", "/").replace(" ", "%20");

        mStage = new Stage();

        Screen screen = Screen.getPrimary();
        Rectangle2D bounds = screen.getVisualBounds();

        Group groupNode = new Group();
        SubScene guiSubScene = new SubScene(groupNode, bounds.getWidth(), bounds.getHeight(), true, SceneAntialiasing.BALANCED);
        guiSubScene.setFill(javafx.scene.paint.Color.TRANSPARENT);
        guiSubScene.setBlendMode(BlendMode.MULTIPLY);
        groupNode.getStylesheets().add(cssResource);

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

            GUIElementValues values = mExecutor.mGUIElementIdValues.get(e);

            if (values.mId.contains("button")) {
                Button button = new Button();
                button.getStyleClass().add("button");
                button.setText(values.mName);
                button.setFont(Font.font(Font.getDefault().getName(), values.mSize));
                button.setTranslateX(values.mX);
                button.setTranslateY(values.mY);
                button.setOnAction(arg0 -> {
                    // set SceneMaker variable
                    if (mExecutor != null) {
                        mExecutor.setVSmVar(values.mVSMVar, values.mValue);
                    }
                    // hide only if option hide on pressed is set true
                    if (mHideOnPressed) {
                        mStage.close();
                        mStage = null;
                    }
                });

                groupNode.getChildren().add(button);
            }

            if (values.mId.contains("image")) {
                HBox canvas = new HBox();

                String imageResource = "file:///" + mExecutor.getProjectPath() + File.separator + values.mValue;
                imageResource = imageResource.replace("\\", "/").replace(" ", "%20");

                Image image = new Image(imageResource);
                ImageView view = new ImageView(image);

                view.setTranslateX(values.mX);
                view.setTranslateY(values.mY);

                groupNode.getChildren().add(view);
            }

            if (values.mId.contains("label")) {
                Label label = new Label();
                //label.getStyleClass().add("label");
                label.setText(values.mName);
                label.setFont(Font.font(Font.getDefault().getName(), values.mSize));
                label.setTranslateX(values.mX);
                label.setTranslateY(values.mY);

                groupNode.getChildren().add(label);
            }

            if (values.mId.contains("rectangle")) {
                Pane pane = new Pane();

                pane.setTranslateX(values.mX);
                pane.setTranslateY(values.mY);
                pane.setPrefSize(values.mSize, Integer.parseInt(values.mName));
                pane.setStyle("-fx-background-color: " + values.mValue);

                groupNode.getChildren().add(pane);
            }

            if (values.mId.contains("textfield")) {
                TextField textfield = new TextField();
                textfield.setText(values.mValue);
                textfield.setFont(Font.font(Font.getDefault().getName(), values.mSize));
                textfield.setTranslateX(values.mX);
                textfield.setTranslateY(values.mY);
                textfield.setOnAction(arg0 -> {
                    // set SceneMaker variable
                    if (mExecutor != null) {
                        mExecutor.setVSmVar(values.mVSMVar, textfield.getText());
                    }
                    // hide only if option hide on pressed is set true
                    if (mHideOnPressed) {
                        mStage.close();
                        mStage = null;
                    }
                });

                groupNode.getChildren().add(textfield);
            }
        }

        // build layout
        StackPane sp = new StackPane();
        StackPane.setAlignment(guiSubScene, Pos.TOP_CENTER);
        sp.getChildren().add(guiSubScene);
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

    void hideGUIElements() {
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
