package de.dfki.vsm.extensionAPI.renderers.customcontrollers;

import de.dfki.vsm.extensionAPI.renderers.customcontrollers.pathchoosers.CustomDirectoryChooser;
import de.dfki.vsm.extensionAPI.renderers.customcontrollers.pathchoosers.PathChooser;
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
        btnOpenDirectoryChooser.setOnAction(event -> {
            File selectedDirectory = pathChooser.showDialog();
            if(selectedDirectory == null){
                labelSelectedDirectory.setText("No Directory selected");
            }else{
                labelSelectedDirectory.setText(selectedDirectory.getAbsolutePath());
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
