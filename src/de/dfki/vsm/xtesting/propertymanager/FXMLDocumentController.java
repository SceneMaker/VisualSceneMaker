package de.dfki.vsm.xtesting.propertymanager;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.model.project.ProjectConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.input.MouseEvent;

import java.io.File;
import java.net.URL;
import java.util.ResourceBundle;

/**
 * Created by alvaro on 4/23/16.
 */
public class FXMLDocumentController implements Initializable {
    /**
     * Called to initialize a controller after its root element has been
     * completely processed.
     *
     * @param location  The location used to resolve relative paths for the root object, or
     *                  <tt>null</tt> if the location is not known.
     * @param resources The resources used to localize the root object, or <tt>null</tt> if
     */
    @FXML private TableView pluginsTable ;
    @FXML private TableView agentsTable ;
    @FXML private TableColumn keyColumn;
    @FXML private TableColumn valueColumn;
    @FXML private TableColumn  pluginColumn ;

    @FXML private TableColumn deviceColumn;
    @FXML private TableColumn agentColumn;
    @FXML private TableColumn  keyAgentColumn ;
    @FXML private TableColumn  valueAgentColumn ;

    @FXML private ComboBox cmbPlugin ;
    @FXML private ComboBox cmbAgent ;

    @FXML private TextField txtKey ;
    @FXML private TextField txtValue ;

    @FXML private TextField txtAgentValue ;
    @FXML private TextField txtAgentKey ;

    private ObservableList<TableConfig> data = FXCollections.observableArrayList();
    private ObservableList<TableConfig> agentData = FXCollections.observableArrayList();

    private final ProjectConfig mConfig;
    private RunTimeProject mProject = null;
    @Override
    public void initialize(URL location, ResourceBundle resources) {
        populatePluginTable();
        populateAgentsTable();
    }

    private void populateAgentsTable(){
        ObservableList<String> agentList = FXCollections.observableArrayList();
        for (AgentConfig agent: mConfig.getAgentConfigList() ) {
            agentList.add(agent.getAgentName());
            for (ConfigFeature feat :agent.getEntryList() ) {
                TableConfig tFeat = new TableConfig(feat.getKey(), feat.getValue(), agent.getDeviceName(), agent.getAgentName());
                agentData.add(tFeat);
            }

        }
        cmbAgent.getItems().addAll(agentList);
        //pluginColumn.setCellValueFactory(new PropertyValueFactory("pluginname"));
        valueAgentColumn.setCellFactory(TextFieldTableCell.forTableColumn());
        valueAgentColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
                    @Override
                    public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                        ((TableConfig) t.getTableView().getItems().get(
                                t.getTablePosition().getRow())
                        ).setValue(t.getNewValue());
                    }
                }
        );
        deviceColumn.setCellValueFactory(new PropertyValueFactory("device"));
        agentColumn.setCellValueFactory(new PropertyValueFactory("agent"));
        keyAgentColumn.setCellValueFactory(new PropertyValueFactory("key"));
        valueAgentColumn.setCellValueFactory(new PropertyValueFactory("value"));
        agentsTable.setItems(agentData);
    }

    private void populatePluginTable(){
        ObservableList<String> pluginList = FXCollections.observableArrayList();
        for (PluginConfig plugin: mConfig.getPluginConfigList() ) {
            pluginList.add(plugin.getPluginName());
            for (ConfigFeature feat :plugin.getEntryList() ) {
                TableConfig tFeat = new TableConfig(feat.getKey(), feat.getValue(), plugin.getPluginName());
                data.add(tFeat);
                System.out.println("Plugin as");
            }

        }
        cmbPlugin.getItems().addAll(pluginList);
        //pluginColumn.setCellValueFactory(new PropertyValueFactory("pluginname"));
        valueColumn.setCellFactory(TextFieldTableCell.forTableColumn());
        valueColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
                    @Override
                    public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                        ((TableConfig) t.getTableView().getItems().get(
                                t.getTablePosition().getRow())
                        ).setValue(t.getNewValue());
                    }
                }
        );
        pluginColumn.setCellValueFactory(new PropertyValueFactory("plugin"));
        keyColumn.setCellValueFactory(new PropertyValueFactory("key"));
        valueColumn.setCellValueFactory(new PropertyValueFactory("value"));
        pluginsTable.setItems(data);
    }

    @FXML
    public void  addPluginRow(MouseEvent event){
        if(!txtKey.getText().equals("") && !txtValue.getText().equals("") && cmbPlugin.getSelectionModel().getSelectedIndex() >= 0) {
            TableConfig dC = new TableConfig(txtKey.getText(), txtValue.getText(), cmbPlugin.getSelectionModel().getSelectedItem().toString());
            data.add(dC);
        }
    }

    @FXML
    public void  addAgentRow(MouseEvent event){
        if(!txtAgentKey.getText().equals("") && !txtAgentValue.getText().equals("") && cmbAgent.getSelectionModel().getSelectedIndex() >= 0) {
            String agent = cmbAgent.getSelectionModel().getSelectedItem().toString();
            AgentConfig agentConfig = mConfig.getAgentConfig(agent);
            if(agentConfig!=null){
                String device = agentConfig.getDeviceName();
                TableConfig dC = new TableConfig(txtAgentKey.getText(), txtAgentValue.getText(), device, agent);
                agentData.add(dC);
            }
        }
    }

    @FXML
    public void saveAll(MouseEvent event){
        for (PluginConfig plugin: mConfig.getPluginConfigList() ) {
            for (TableConfig tc: data){
                String oldValue = plugin.getProperty(tc.getKey());
                if(oldValue!=null && !oldValue.equals(tc.getValue()) && tc.getPlugin().equals(plugin.getPluginName())){ //Case that the key value pair was changed
                    plugin.setProperty(tc.getKey(), tc.getValue());
                }else if((oldValue == null || oldValue.equals("")) && tc.getPlugin().equals(plugin.getPluginName())){
                    plugin.setProperty(tc.getKey(), tc.getValue());
                }
            }


        }

        for (AgentConfig agent: mConfig.getAgentConfigList() ) {

            for (TableConfig tc: agentData){
                String oldValue = agent.getProperty(tc.getKey());
                if(oldValue!=null && !oldValue.equals(tc.getValue()) && tc.getDevice().equals(agent.getDeviceName()) && tc.getAgent().equals(agent.getAgentName())){ //Case that the key value pair was changed
                    agent.setProperty(tc.getKey(), tc.getValue());
                }else if((oldValue == null || oldValue.equals("")) && tc.getDevice().equals(agent.getDeviceName()) && tc.getAgent().equals(agent.getAgentName()) ){
                    agent.setProperty(tc.getKey(), tc.getValue());
                }
            }

        }

        //TODO: Remove later
        File f = new File("/home/alvaro/Documents/WorkHiwi/VSM_stable/VisualSceneMaker/res/tutorials/6.MaryTTS");
        mProject.write(f);

    }
    public FXMLDocumentController(ProjectConfig config){
        super();
        mConfig = config;
    }

    public FXMLDocumentController(ProjectConfig config, RunTimeProject project){
        super();
        mConfig = config;
        mProject = project;
    }

}

