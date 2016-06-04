package de.dfki.vsm.xtesting.NewPropertyManager;
import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.xtesting.NewPropertyManager.model.*;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.AgentTableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.PluginTableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.TableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.util.*;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.util.Callback;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by alvaro on 6/2/16.
 */
public class PropertyManagerController implements Initializable, TreeObserver {

    private RunTimeProject mProject = null;
    @FXML
    TreeView<AbstractTreeEntry> treeView;
    @FXML private TableView pluginsTable ;
    @FXML private TableColumn keyColumn;
    @FXML private TableColumn valueColumn;
    @FXML private TextField txtKey;
    @FXML private TextField txtValue ;
    @FXML private Button btnAddEntry ;
    @FXML private ComboBox cmbExecutor;
    @FXML private Button btnAddDevice;
    @FXML private TextField txtDeviceName;
    @FXML private Label lblClassName;
    @FXML private CheckBox chkLoadPlugin;
    @FXML private Pane addDevice;
    @FXML private AnchorPane editDevice;
    @FXML private VBox InfoVBox;
    private  ArrayList <String> activityClassesShortNames;
    private  ArrayList <String> activityClassesLongNames;
    private TreeItem<AbstractTreeEntry> devices;
    private HashMap<String, PluginConfig> plugins= new HashMap<>();
    private EntryDevice entryDevice;
    private ObservableList<TableConfig> data = FXCollections.observableArrayList();
    private ProjectConfigWrapper projectConfigWrapper;


    public PropertyManagerController(RunTimeProject project){
        super();
        projectConfigWrapper = new ProjectConfigWrapper(project);
        entryDevice = new EntryDevice("Devices");
        mProject = project;
        PluginConfig plugin = null;
        Iterator it = mProject.getProjectConfig().getPluginConfigList().iterator();
        while (it.hasNext() && plugin == null) {
            PluginConfig p = (PluginConfig) it.next();
            plugins.put(p.getPluginName(), p);
        }
    }

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        TreeItem<AbstractTreeEntry> root = new TreeItem<>(new EntryRoot("VSM Config"));
        devices = new TreeItem<>(new EntryDevice("Devices"));
        initializePlugins();
        addInitialPluginsToTreeList();
        root.getChildren().add(devices);
        treeView.setRoot(root);
        treeView.setEditable(true);
        final PropertyManagerController controller = this;
        fillComboWithActivityExecutors();
        treeView.setCellFactory(new Callback<TreeView<AbstractTreeEntry>,TreeCell<AbstractTreeEntry>>(){
            @Override
            public TreeCell<AbstractTreeEntry> call(TreeView<AbstractTreeEntry> param) {
                TreeCellImpl treeCell = new TreeCellImpl();
                treeCell.registerObserver(controller);
                return treeCell;
            }
        });
    }

    private void fillComboWithActivityExecutors(){
        ExtensionsFromJar extensions = new ExtensionsFromJar("de.dfki.vsm.xtension", false);
        extensions.loadClass();
        ObservableList<String> devicesNames = FXCollections.observableArrayList();
        activityClassesShortNames =  extensions.getActivitiesShortNames();
        activityClassesLongNames = extensions.getActivitiesLongName();
        devicesNames.addAll(activityClassesShortNames.stream().collect(Collectors.toList()));
        cmbExecutor.getItems().addAll(devicesNames);
    }

    private void initializePlugins(){
        for (PluginConfig plugin:  mProject.getProjectConfig().getPluginConfigList()) {
            EntryPlugin entryPlugin = new EntryPlugin(plugin);
            addAgentsToPlugin(entryPlugin, plugin.getPluginName());
            entryDevice.addPlugin(entryPlugin);
        }
    }

    private void addAgentsToPlugin(EntryPlugin entryPlugin, String pluginName){
        for(AgentConfig agent: mProject.getProjectConfig().getAgentConfigList() ) {
            if(agent.getDeviceName().equals(pluginName)){
                EntryAgent entryAgent = new EntryAgent(agent);
                entryPlugin.addAgent(entryAgent);
            }
        }
    }

    private void addInitialPluginsToTreeList(){
        entryDevice.getPlugins().forEach(this::addPluginToList);

    }

    private void addAgentNodeToPluginNode(EntryPlugin entryPlugin, ContextTreeItem pluginNode){
        for(EntryAgent agent: entryPlugin.getAgents() ) {
            ContextTreeItem agentNode = new ContextTreeItem(agent); //It should not be ContextTreeItem
            agentNode.registerObserver(this);
            pluginNode.getChildren().add(agentNode);
        }
    }

    @FXML
    public void addNewItemToTableEvent(ActionEvent event){
        if(!txtKey.getText().equals("") && !txtValue.getText().equals("")) {
            addNewItemToTable();
        }
    }

    @FXML
    public void addDevice(){
        String deviceName = txtDeviceName.getText();
        String className = activityClassesLongNames.get( cmbExecutor.getSelectionModel().getSelectedIndex());
        boolean added = projectConfigWrapper.addNewPlugin(deviceName, className);
        if(added){
            PluginConfig plugin = mProject.getPluginConfig(deviceName);
            EntryPlugin entryPlugin = new EntryPlugin(plugin);
            entryDevice.addPlugin(entryPlugin);
            addPluginToList(entryPlugin);

        }
    }

    private void addPluginToList(EntryPlugin entryPlugin){
        ContextTreeItem pluginNode = new ContextTreeItem(entryPlugin);
        pluginNode.registerObserver(this);
        addAgentNodeToPluginNode(entryPlugin, pluginNode);
        devices.getChildren().add(pluginNode);
    }

    private void addNewItemToTable(){
        try {
            addTableConfigItem();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private  void addTableConfigItem() throws Exception {
        AbstractTreeEntry selectedItem = getSelectedTreeItem(false);
        if(selectedItem instanceof EntryAgent){
            EntryAgent agent = (EntryAgent) selectedItem;
            addNewAgentToObservableTableAndSave(agent);
        }
        if(selectedItem instanceof EntryPlugin){
            EntryPlugin plugin = (EntryPlugin) selectedItem;
            addNewDeviceToObservableTableAndSave(plugin);
        }
    }

    private void addNewAgentToObservableTableAndSave(EntryAgent agent){
        AgentTableConfig dC = new AgentTableConfig(txtKey.getText(), txtValue.getText(), agent.getAgentConfig());
        data.add(dC);
        dC.saveEntry();
        saveConfig();
    }

    private void addNewDeviceToObservableTableAndSave(EntryPlugin plugin){
        PluginTableConfig dC = new PluginTableConfig(txtKey.getText(), txtValue.getText(), plugin.getPluginConfig());
        data.add(dC);
        dC.saveEntry();
        saveConfig();
    }

    @FXML
    public void selectConfig(MouseEvent event){
        try {
           processClickedTreeElement(event.getButton().toString().equals("SECONDARY"));
        } catch (Exception e) {
          System.out.println(e);
        }
    }

    @FXML
    public void handleLoadPluginCheckbox(){
        boolean loaded = false;
        if(chkLoadPlugin.isSelected()){
            loaded = true;
        }
        try {
            changeLoadPluginStatus(loaded);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    private void changeLoadPluginStatus(boolean loaded) throws Exception {
        AbstractTreeEntry itemEntry;
        itemEntry = getSelectedTreeItem(false);
        if(itemEntry instanceof EntryPlugin){
            ((EntryPlugin) itemEntry).getPluginConfig().setLoad(loaded);
        }else if(itemEntry instanceof EntryAgent){
            String pluginName = ((EntryAgent) itemEntry).getPluginName();
            mProject.getPluginConfig(pluginName).setLoad(loaded);
        }
        saveConfig();
    }

    private void processClickedTreeElement(boolean isRightClicked) throws Exception {
        AbstractTreeEntry itemEntry = null;
        itemEntry = getSelectedTreeItem(isRightClicked);
        if(itemEntry instanceof EntryDevice){
            showAddDevice();
        }
        if(itemEntry instanceof EntryPlugin){
            EntryPlugin entryPlugin = (EntryPlugin) itemEntry;
            showPluginDatainTable(entryPlugin);
            setLoadPluginCheckbox(entryPlugin.getPluginConfig());
            hideAddDevice();
        }
        if(itemEntry instanceof EntryAgent){
            EntryAgent entryAgent= (EntryAgent) itemEntry;
            showAgentDatainTable(entryAgent);
            String pluginName = entryAgent.getPluginName();
            setLoadPluginCheckbox(mProject.getPluginConfig(pluginName));
            hideAddDevice();
        }
    }

    private void setLoadPluginCheckbox(PluginConfig plugin){
        chkLoadPlugin.setSelected(plugin.isMarkedtoLoad());
    }

    private AbstractTreeEntry getSelectedTreeItem(boolean isRightClicked) throws Exception {
        Object selectedItem = treeView.getSelectionModel().getSelectedItem();
        if(selectedItem == null){
           throw  new Exception("Not selected item");
        }
        AbstractTreeEntry itemEntry;
        if(selectedItem instanceof  ContextTreeItem){
            itemEntry = getContextItemSelected(isRightClicked);
        }else if(selectedItem instanceof BoxTreeItem){
            itemEntry = (AbstractTreeEntry) ((BoxTreeItem) selectedItem).getValue();
        }else if(selectedItem instanceof AbstractTreeItem){
            itemEntry = (AbstractTreeEntry) selectedItem;
        }else if(selectedItem instanceof TreeItem && ((TreeItem)selectedItem).getValue() instanceof EntryDevice){
            itemEntry = (AbstractTreeEntry) ((TreeItem) selectedItem).getValue();
        }
        else {
            System.out.println("NO Datatype matched!");
            throw  new Exception("Non datatype recognized");
        }
        return itemEntry;
    }

    private AbstractTreeEntry getContextItemSelected(boolean isRightClicked){
        Object selectedItem = treeView.getSelectionModel().getSelectedItem();
        if(isRightClicked){
            ((ContextTreeItem)selectedItem).getMenu();
            new ContextMenu(new MenuItem("HOLA"));
        }
        return ((ContextTreeItem) selectedItem).getEntryItem();
    }

    private void showPluginDatainTable(EntryPlugin entryPlugin){
        showPluginTable(entryPlugin);
        setClassNameLabel(entryPlugin.getPluginClassName());
    }

    private void showAgentDatainTable(EntryAgent entryAgent){
        showAgentTable(entryAgent);
        String pluginName = entryAgent.getPluginName();
        setClassNameLabel(mProject.getPluginConfig(pluginName).getClassName());
    }

    private void showAddDevice(){
        editDevice.setVisible(false);
        InfoVBox.setVisible(false);
        addDevice.setVisible(true);
    }

    private void hideAddDevice(){
        editDevice.setVisible(true);
        InfoVBox.setVisible(true);
        addDevice.setVisible(false);
    }

    private void setClassNameLabel(String className){
        lblClassName.setText("Class: " + className);
    }

    private void showPluginTable(EntryPlugin plugin){
        pluginsTable.setEditable(true);
        getPluginData(plugin.getPluginConfig());
    }

    private void showAgentTable(EntryAgent agent){
        pluginsTable.setEditable(true);
        getAgentData(agent.getAgentConfig());
    }

    private void getPluginData(PluginConfig plugin){
        data.clear();
        for (ConfigFeature feat : plugin.getEntryList()) {
            PluginTableConfig tFeat = new PluginTableConfig(feat.getKey(), feat.getValue(), plugin);
            data.add(tFeat);
        }
        populatePluginTable(plugin.getPluginName());
    }

    private void getAgentData(AgentConfig agent){
        data.clear();
        for (ConfigFeature feat: agent.getEntryList()    ) {
            AgentTableConfig tFeat = new AgentTableConfig(feat.getKey(), feat.getValue(), agent);
            data.add(tFeat);
        }
        populatePluginTable(agent.getDeviceName());
    }

    private void populatePluginTable(String pluginName){
        ObservableList<String> pluginList = FXCollections.observableArrayList();
        setEditableValueCell(pluginName);
        setEditablKeyeCell(pluginName);
        pluginsTable.setItems(data);
    }

    private void setEditablKeyeCell(String pluginName){
        keyColumn.setCellFactory(TextFieldTableCell.forTableColumn());

        keyColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
                    @Override
                    public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                        TableConfig dataConfig = t.getRowValue();
                        if(t.getOldValue() != t.getNewValue()){
                            removeOldProperty(dataConfig, t.getOldValue());
                        }

                        ((TableConfig) t.getTableView().getItems().get(
                                t.getTablePosition().getRow())
                        ).setKey(t.getNewValue());


                        saveEditedTableEntry(dataConfig);


                    }
                }
        );
        keyColumn.setCellValueFactory(new PropertyValueFactory("key"));
    }

    private void setEditableValueCell(String pluginName){
        valueColumn.setCellFactory(TextFieldTableCell.forTableColumn());
        valueColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
                    @Override
                    public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                        ((TableConfig) t.getTableView().getItems().get(
                                t.getTablePosition().getRow())
                        ).setValue(t.getNewValue());
                        TableConfig dataConfig = t.getRowValue();
                       saveEditedTableEntry(dataConfig);
                    }
                }
        );
        valueColumn.setCellValueFactory(new PropertyValueFactory("value"));
    }


    private void saveEditedTableEntry(TableConfig dataConfig){
        if(dataConfig instanceof AgentTableConfig){
            ((AgentTableConfig) dataConfig).saveEntry();
        }
        if(dataConfig instanceof PluginTableConfig){
            ((PluginTableConfig) dataConfig).saveEntry();
        }
        saveConfig();
    }

    private void removeOldProperty(TableConfig dataConfig, String oldValue){
        if(dataConfig instanceof AgentTableConfig){
            ((AgentTableConfig) dataConfig).removeProperty(oldValue);
        }
        if(dataConfig instanceof PluginTableConfig){
            ((PluginTableConfig) dataConfig).removeProperty(oldValue);
        }
    }

    public void saveConfig(){
        projectConfigWrapper.saveConfig();
    }


    @Override
    public void update(NotificationObject object) {
        if(object instanceof ContextEvent){//This called when a new agent is added via context menu
            String agent = ((ContextEvent) object).getContextName();
            String pluginName = ((ContextEvent) object).getPluginName();
            EntryAgent agentEntry = (EntryAgent) ((ContextEvent) object).getTreeEntry();
            projectConfigWrapper.addNewAgent(agent, pluginName);
            agentEntry.setAgentConfig(mProject.getAgentConfig(agent));
        } else if(object instanceof CellEvent){//We change either the name of the agent or device
            changeItemName((CellEvent) object);
        }
    }

    private void changeItemName(CellEvent event){
        String oldValue = ((CellEvent) event).getOldValue();
        String newValue = ((CellEvent) event).getNewValue();
        AbstractTreeEntry entry = ((CellEvent) event).getTreeEntry();
        if(entry instanceof EntryPlugin){
            changePluginName((EntryPlugin) entry, oldValue, newValue);

        }
        if(entry instanceof EntryAgent){
            changeAgentName((EntryAgent) entry, oldValue, newValue);
        }
    }

    private void changePluginName(EntryPlugin plugin, String oldValue, String newValue){
        try {
            projectConfigWrapper.changePluginName(plugin.getPluginConfig(), oldValue, newValue);
            plugin.setPluginConfig(mProject.getPluginConfig(newValue));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void changeAgentName(EntryAgent agent, String oldValue, String newValue){
        try {
            projectConfigWrapper.changeAgentName(oldValue, newValue);
            agent.setAgentConfig(mProject.getAgentConfig(newValue));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}