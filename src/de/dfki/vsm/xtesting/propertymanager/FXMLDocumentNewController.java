package de.dfki.vsm.xtesting.propertymanager;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;


import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Collectors;

/**
 * Created by alvaro on 4/23/16.
 */
public class FXMLDocumentNewController implements Initializable {

    private RunTimeProject mProject = null;
    @FXML
    TreeView<String> treeView;
    @FXML
    AnchorPane contentPane;
    @FXML private TableView pluginsTable ;
    @FXML private TableColumn keyColumn;
    @FXML private TableColumn valueColumn;
    @FXML private TextField txtKey;
    @FXML private TextField txtValue ;
    @FXML private Button btnAddEntry ;
    @FXML private ComboBox cmbExecutor;
    @FXML private Button btnAddDevice;
    @FXML private TextField txtDeviceName;
    private TreeItem<String> devices;
    private HashMap<String, PluginConfig> plugins= new HashMap<>();

    private static ArrayList<String> mScenePlayersShortNames = new ArrayList<>();
    private static ArrayList<String> mScenePlayersLongNames = new ArrayList<>();

    private ObservableList<TableConfig> data = FXCollections.observableArrayList();
    public void initialize(URL location, ResourceBundle resources) {
        TreeItem<String> root = new TreeItem<>("VSM Config");
        devices = new TreeItem<>("Devices");

        for (PluginConfig plugin:  mProject.getProjectConfig().getPluginConfigList()) {
            TreeItem<String> pluginNode = new TreeItem<>(plugin.getPluginName());
            for  (AgentConfig agent: mProject.getProjectConfig().getAgentConfigList() ) {
                if(agent.getDeviceName().equals(plugin.getPluginName())){
                    TreeItem<String> agentNode = new TreeItem<>(agent.getAgentName());
                    pluginNode.getChildren().add(agentNode);
                }
            }
            devices.getChildren().add(pluginNode);

        }
        root.getChildren().add(devices);
        treeView.setRoot(root);
    }

    public FXMLDocumentNewController(RunTimeProject project){
        super();
        mProject = project;
        PluginConfig plugin = null;
        Iterator it = mProject.getProjectConfig().getPluginConfigList().iterator();
        while (it.hasNext() && plugin == null) {
            PluginConfig p = (PluginConfig) it.next();
            plugins.put(p.getPluginName(), p);
        }


    }

    private void showAddDevice(){
        Pane p = (Pane)contentPane.lookup("#editDevice");
        if(p!= null){
            Pane content = null;
            if(mScenePlayersShortNames.size()<=0){
                loadClass();
            }
            contentPane.getChildren().remove(p);
            try {
                ObservableList<String> devicesNames = FXCollections.observableArrayList();
                content = (Pane) FXMLLoader.load(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLAddDevice.fxml"));
                cmbExecutor = (ComboBox) content.lookup("#cmbExecutor");
                btnAddDevice = (Button) content.lookup("#btnAddDevice");
                txtDeviceName = (TextField) content.lookup("#txtDeviceName");
                btnAddDevice.setOnAction(new EventHandler<ActionEvent>() {
                    @Override
                    public void handle(ActionEvent event) {
                        boolean added = addDevice(event);
                        if(added){
                            TreeItem<String> pluginNode = new TreeItem<>(txtDeviceName.getText());
                            devices.getChildren().add(pluginNode);
                        }
                    }
                });
                devicesNames.addAll(mScenePlayersShortNames.stream().collect(Collectors.toList()));
                cmbExecutor.getItems().addAll(devicesNames);
                contentPane.getChildren().addAll(content);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private void showPluginTable(PluginConfig plugin, TreeItem<String> item, String type){
        Pane content = null;
        try {
            Pane p = (Pane)contentPane.lookup("#addDevice");
            Pane pEdit = (Pane)contentPane.lookup("#editDevice");
            if(p!= null){
                contentPane.getChildren().remove(p);
            }
            if(pEdit!= null){
                contentPane.getChildren().remove(pEdit);
            }
            content = (Pane) FXMLLoader.load(getClass().getResource("/res/de/dfki/vsm/xtesting/propertymanager/FXMLEditDevice.fxml"));
            pluginsTable = (TableView) content.lookup("#pluginsTable");
            txtKey = (TextField) content.lookup("#txtKey");
            txtValue = (TextField) content.lookup("#txtValue");
            keyColumn = (TableColumn) pluginsTable.getColumns().get(0);
            valueColumn = (TableColumn) pluginsTable.getColumns().get(1);
            btnAddEntry = (Button) content.lookup("#btnAddEntry");
            btnAddEntry.setOnAction(new EventHandler<ActionEvent>() {
                @Override
                public void handle(ActionEvent event) {
                    if(!txtKey.getText().equals("") && !txtValue.getText().equals("")) {
                        TableConfig dC = new TableConfig(txtKey.getText(), txtValue.getText());
                        data.add(dC);
                    }
                }
            });
            pluginsTable.setEditable(true);

            if(type.equals("Plugins")) {
                getPluginData(plugin);
            }else if(type.equals("Agents")) {
                getAgentData(plugin, item.getValue());
            }

            //populatePluginTable(plugin, type);
        } catch (IOException e) {
            e.printStackTrace();
        }

        contentPane.getChildren().addAll(content);
    }

    @FXML
    private void selectConfig(MouseEvent event) {
        TreeItem<String> item = treeView.getSelectionModel().getSelectedItem();
        Iterator it = mProject.getProjectConfig().getPluginConfigList().iterator();
        System.out.println(item);
        PluginConfig plugin = getSelectedPlugin(item, it);
        if(plugin == null && item!=null && item.getValue().equals("Devices")){
            showAddDevice();
        }
        else if(plugin!= null){
            showPluginTable(plugin, item, "Plugins");

        }
        else if(item!=null && item.getParent() != null && plugins.containsKey(item.getParent().getValue())){//Its a plugin
            showPluginTable(plugins.get(item.getParent().getValue()), item, "Agents");
        }
    }

    private PluginConfig getSelectedPlugin(TreeItem<String> item, Iterator it) {
        PluginConfig plugin = null;
        if(item!=null) {
            while (it.hasNext() && plugin == null) {
                PluginConfig p = (PluginConfig) it.next();
                if (p.getPluginName().equals(item.getValue())) {
                    plugin = p;
                }
            }
        }
        return plugin;
    }

    private void getPluginData(PluginConfig plugin){
        data.clear();
        for (ConfigFeature feat : plugin.getEntryList()) {
            TableConfig tFeat = new TableConfig(feat.getKey(), feat.getValue(), plugin.getPluginName());
            data.add(tFeat);
            System.out.println("Plugin as");
        }
        populatePluginTable(plugin);
    }

    private void getAgentData(PluginConfig plugin, String agentName){
        data.clear();
        for  (AgentConfig agent: mProject.getProjectConfig().getAgentConfigList() ) {
            if(agent.getDeviceName().equals(plugin.getPluginName()) && agentName.equals(agent.getAgentName())){
                for (ConfigFeature feat :agent.getEntryList() ) {
                    TableConfig tFeat = new TableConfig(feat.getKey(), feat.getValue());
                    data.add(tFeat);
                }

            }
        }
        populatePluginTable(plugin);
    }

    private void populatePluginTable(PluginConfig plugin){

        ObservableList<String> pluginList = FXCollections.observableArrayList();

        //pluginColumn.setCellValueFactory(new PropertyValueFactory("pluginname"));
        keyColumn.setCellFactory(TextFieldTableCell.forTableColumn());
        keyColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
                    @Override
                    public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                        ((TableConfig) t.getTableView().getItems().get(
                                t.getTablePosition().getRow())
                        ).setValue(t.getNewValue());
                    }
                }
        );

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
        keyColumn.setCellValueFactory(new PropertyValueFactory("key"));
        valueColumn.setCellValueFactory(new PropertyValueFactory("value"));
        pluginsTable.setItems(data);
    }


    private boolean addDevice(ActionEvent event){
        if (cmbExecutor.getSelectionModel().getSelectedIndex() > 0 && !txtDeviceName.getText().equals("") )
        { // 0 Is for the default configuration

            String newPlayer = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                    + "<Project name=\"" + mProject.getProjectName() + "\">"
                    + "<Plugins>"
                    + "<Plugin type=\"device\" name=\"" + txtDeviceName.getText() + "\" class=\"" + mScenePlayersLongNames.get( cmbExecutor.getSelectionModel().getSelectedIndex() - 1) + "\">"
                    + "</Plugin>"
                    + "</Plugins>"
                    + "</Project>";

            return mProject.parseProjectConfigFromString(newPlayer);
        }
        return false;
    }

    public static void loadClass()
    {
        try
        {
            getClassNamesFromPackage("de.dfki.vsm.xtension");
        } catch (IOException e)
        {
            e.printStackTrace();
        }
    }


    private static void parseJar(String jarFileName, String packageName){
        JarFile jf;
        Enumeration<JarEntry> jarEntries = null;
        String entryName;
        try {
            jf = new JarFile(jarFileName);
            jarEntries = jf.entries();
        } catch (IOException e) {
            e.printStackTrace();
        }


        while (jarEntries.hasMoreElements())
        {
            entryName = jarEntries.nextElement().getName();
            if (packageName.length() == 0 || (entryName.startsWith(packageName) && entryName.length() > packageName.length() + 5))
            {
                try
                {
                    String fullClassName = entryName.replace("/", ".");
                    entryName = fullClassName.substring(0, entryName.lastIndexOf('.'));
                    Class classEntry = Class.forName(entryName);
                    //Class[] interfaces = classEntry.getInterfaces();
                    Class t =classEntry.getSuperclass();
                    if(t != null && t.getSimpleName().equals("ActivityExecutor")){
                        mScenePlayersLongNames.add(entryName);
                        mScenePlayersShortNames.add(entryName.substring(entryName.lastIndexOf('.') + 1));
                    }



                } catch (StringIndexOutOfBoundsException e)
                {
                    System.out.println(entryName);
                    continue;
                } catch (ClassNotFoundException e)
                {
                    System.out.println("Class not found");
                    continue;
                }

            }
        }
    }

    public static void getClassNamesFromPackage(String packageName) throws IOException
    {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL packageURL;
        ArrayList<String> names = new ArrayList<String>();;

        packageName = packageName.replace(".", "/");
        packageURL = classLoader.getResource(packageName);
        if (mScenePlayersShortNames.size() <= 0)
        {
            mScenePlayersShortNames.add("Non selected");

            if (packageURL.getProtocol().equals("jar"))
            {
                String jarFileName;
                // build jar file name, then loop through zipped entries
                jarFileName = URLDecoder.decode(packageURL.getFile(), "UTF-8");
                jarFileName = jarFileName.substring(5, jarFileName.indexOf("!"));
                System.out.println(">" + jarFileName);
                parseJar(jarFileName, packageName);

                // loop through files in classpath
            }
            else
            {
                URI uri = null;
                try
                {
                    uri = new URI(packageURL.toString());
                } catch (URISyntaxException e) {
                    e.printStackTrace();
                }
                File folder = new File(uri.getPath());
                // won't work with path which contains blank (%20)
                // File folder = new File(packageURL.getFile());
                File[] contenuti = folder.listFiles();
                String entryName;
                for (File actual : contenuti)
                {
                    entryName = actual.getName();
                    entryName = entryName.substring(0, entryName.lastIndexOf('.'));
                    names.add(entryName);
                }
            }
        }
    }



}

