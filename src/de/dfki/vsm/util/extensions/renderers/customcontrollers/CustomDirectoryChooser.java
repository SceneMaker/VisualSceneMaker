package de.dfki.vsm.util.extensions.renderers.customcontrollers;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.DirectoryChooser;

import java.io.File;

/**
 * Created by alvaro on 4/23/17.
 */
public class CustomDirectoryChooser {

    private TextField labelSelectedDirectory;

    public Node getControl(){
        labelSelectedDirectory = new TextField();
        Button btnOpenDirectoryChooser = new Button();
        btnOpenDirectoryChooser.setText("...");
        btnOpenDirectoryChooser.setOnAction(new EventHandler<ActionEvent>() {

            @Override
            public void handle(ActionEvent event) {
                DirectoryChooser directoryChooser = new DirectoryChooser();
                File selectedDirectory = directoryChooser.showDialog(null);

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
