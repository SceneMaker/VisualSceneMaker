package de.dfki.vsm.util.extensions.renderers.customcontrollers;

import de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers.*;
import de.dfki.vsm.util.extensions.renderers.customcontrollers.pathchoosers.CustomDirectoryChooser;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;

import java.io.File;

/**
 * Created by alvaro on 4/23/17.
 */
public class CustomPathChooser {

    private TextField labelSelectedDirectory;
    private PathChooser pathChooser;

    public CustomPathChooser(){
        pathChooser = new CustomDirectoryChooser();
    }

    public CustomPathChooser(PathChooser chooser){
        pathChooser = chooser;
    }

    public Node getControl(){
        labelSelectedDirectory = new TextField();
        Button btnOpenDirectoryChooser = new Button();
        btnOpenDirectoryChooser.setText("...");
        btnOpenDirectoryChooser.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                File selectedDirectory = pathChooser.showDialog();
                if(selectedDirectory == null){
                    labelSelectedDirectory.setText("No Directory selected");
                }else{
                    labelSelectedDirectory.setText(selectedDirectory.getAbsolutePath());
                }
            }
        });

        HBox hBox = new HBox();
        hBox.getChildren().addAll(labelSelectedDirectory, btnOpenDirectoryChooser);
        return hBox;
    }

    public String getFilePath(){
        return  labelSelectedDirectory.getText();
    }


}
