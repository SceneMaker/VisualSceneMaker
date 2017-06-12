package de.dfki.vsm.xtesting.NewPropertyManager;

import de.dfki.vsm.model.config.ConfigFeature;
import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.extensions.ProjectProperty;
import de.dfki.vsm.util.extensions.value.ProjectValueProperty;
import de.dfki.vsm.util.extensions.value.ValueRenderable;
import de.dfki.vsm.xtesting.NewPropertyManager.exceptions.NotExportableInterface;
import de.dfki.vsm.xtesting.NewPropertyManager.model.*;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.AgentTableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.PluginTableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.model.tableView.TableConfig;
import de.dfki.vsm.xtesting.NewPropertyManager.util.*;
import de.dfki.vsm.xtesting.NewPropertyManager.util.events.*;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.util.Callback;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by alvaro on 6/2/16.
 */
public class PropertyManagerController implements Initializable, TreeObserver {

    public static final String FX_TEXT_BOX_BORDER_RED = "-fx-text-box-border: red";
    public static final int DYNAMIC_CONTROL_POSITION = 1;
    public static final int TOTAL_CONTROLS_IN_BASIC_BAR = 4;
    private ExportableClassInitializer exportableClassInitializer;
    private RunTimeProject mProject = null;
    @FXML
    TreeView<AbstractTreeEntry> treeView;
    @FXML
    private TableView pluginsTable;
    @FXML
    private TableColumn keyColumn;
    @FXML
    private TableColumn valueColumn;
    @FXML
    private TextField txtKey;
    @FXML
    private TextField txtValue;
    @FXML
    private Button btnAddEntry;
    @FXML
    private ComboBox cmbExecutor;
    @FXML
    private Button btnAddDevice;
    @FXML
    private TextField txtDeviceName;
    @FXML
    private Label lblClassName;
    @FXML
    private CheckBox chkLoadPlugin;
    @FXML
    private Pane addDevice;
    @FXML
    private AnchorPane editDevice;
    @FXML
    private VBox InfoVBox;
    @FXML
    private ChoiceBox propertiesChooser;
    @FXML
    private HBox advanceBar;
    @FXML
    private HBox basicBar;
    @FXML
    private Button btnAddAdvanced;
    @FXML
    private Button advancedButton;
    @FXML
    private Label descriptionLabel;
    @FXML
    private Button deleteButton;

    private ArrayList<String> activityClassesShortNames;
    private ArrayList<String> activityClassesLongNames;
    private TreeItem<AbstractTreeEntry> devices;
    private HashMap<String, PluginConfig> plugins = new HashMap<>();
    private EntryDevice entryDevice;
    private ObservableList<TableConfig> data = FXCollections.observableArrayList();
    private ProjectConfigWrapper projectConfigWrapper;
    private HashMap<ProjectProperty, ProjectValueProperty> exportableProperties;
    private Boolean advancedBottonMark = false;

    public PropertyManagerController(RunTimeProject project) {
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
        String rootName = getRootName();
        TreeItem<AbstractTreeEntry> root = new TreeItem<>(new EntryRoot(rootName));
        root.setExpanded(true);
        devices = new TreeItem<>(new EntryDevice("Devices"));
        devices.setExpanded(true);
        initializePlugins();
        addInitialPluginsToTreeList();
        root.getChildren().add(devices);
        treeView.setRoot(root);
        treeView.setEditable(true);
        setColumnsSameWidth();
        final PropertyManagerController controller = this;
        fillComboWithActivityExecutors();
        advanceBar.setVisible(false);

        treeView.setCellFactory(new Callback<TreeView<AbstractTreeEntry>, TreeCell<AbstractTreeEntry>>() {
            @Override
            public TreeCell<AbstractTreeEntry> call(TreeView<AbstractTreeEntry> param) {
                TreeCellImpl treeCell = new TreeCellImpl();
                treeCell.registerObserver(controller);
                return treeCell;
            }
        });

        advancedButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                if (!advancedBottonMark) {
                    advancedBottonMark = true;
                    advancedButton.setText("Basic");
                    advanceBar.setVisible(true);
                    basicBar.setVisible(false);

                } else {
                    advancedBottonMark = false;
                    advancedButton.setText("Advanced");
                    advanceBar.setVisible(false);
                    basicBar.setVisible(true);
                }
            }
        });

        btnAddAdvanced.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                ProjectProperty property = (ProjectProperty) propertiesChooser.getSelectionModel().getSelectedItem();
                if (!hasProperty(property)) {
                    return;
                }
                ProjectValueProperty value = exportableProperties.get(property);
                addNewItemToTable(property.getName(), value.getValue());
            }
        });

        deleteButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                int selectedIndex = pluginsTable.getSelectionModel().getSelectedIndex();
                if (selectedIndex >= 0) {
                    TableConfig dataConfig1 = (TableConfig) pluginsTable.getSelectionModel().getSelectedItem();
                    removeOldProperty(dataConfig1, dataConfig1.getKey());
                    pluginsTable.getItems().remove(selectedIndex);
                    saveConfig();
                } else {
                    // Nothing selected.
                }
            }
        });

        propertiesChooser.getSelectionModel().selectedItemProperty().addListener(new ChangeListener() {
            @Override
            public void changed(ObservableValue observable, Object oldValue, Object newValue) {
                ProjectProperty property = (ProjectProperty) newValue;
                if (!hasProperty(property)) {
                    return;
                }
                replaceControlInBasicBar(property);

            }
        });

        // Listen for selection changes and show the person details when changed.
//        pluginsTable.getSelectionModel().selectedItemProperty().addListener(
//                (observable, oldValue, newValue) -> show(newValue));
    }

    private void replaceControlInBasicBar(ProjectProperty property) {
        ProjectValueProperty value = exportableProperties.get(property);
        value.render();
        ValueRenderable renderer = value.getRenderer();
        Node control = renderer.getRenderer();
        if (basicBar.getChildren().size() == TOTAL_CONTROLS_IN_BASIC_BAR) {
            basicBar.getChildren().remove(DYNAMIC_CONTROL_POSITION);
        }
        basicBar.getChildren().add(DYNAMIC_CONTROL_POSITION, control);
        basicBar.setSpacing(10);
        descriptionLabel.setText(property.getDescription());
    }

    private boolean hasProperty(ProjectProperty property) {
        return exportableProperties != null && exportableProperties.containsKey(property);
    }

    private void setColumnsSameWidth() {
//        keyColumn.prefWidthProperty().bind(pluginsTable.widthProperty().divide(2));
//        valueColumn.prefWidthProperty().bind(pluginsTable.widthProperty().divide(2));
    }

    private String getRootName() {
        String name = mProject.getProjectName();
        if (name.isEmpty()) {
            name = "VSM Config";
        }
        return name;
    }

    private void fillComboWithActivityExecutors() {
        ExtensionsFromJar extensions = new ExtensionsFromJar("de.dfki.vsm.xtension", false);
        extensions.loadClass();
        ObservableList<String> devicesNames = FXCollections.observableArrayList();
        activityClassesShortNames = extensions.getActivitiesShortNames();
        activityClassesLongNames = extensions.getActivitiesLongName();
        devicesNames.addAll(activityClassesShortNames.stream().collect(Collectors.toList()));
        cmbExecutor.getItems().addAll(devicesNames);
    }

    private void initializePlugins() {
        for (PluginConfig plugin : mProject.getProjectConfig().getPluginConfigList()) {
            EntryPlugin entryPlugin = new EntryPlugin(plugin);
            addAgentsToPlugin(entryPlugin, plugin.getPluginName());
            entryDevice.addPlugin(entryPlugin);
        }
    }

    private void addAgentsToPlugin(EntryPlugin entryPlugin, String pluginName) {
        for (AgentConfig agent : mProject.getProjectConfig().getAgentConfigList()) {
            if (agent.getDeviceName().equals(pluginName)) {
                EntryAgent entryAgent = new EntryAgent(agent);
                entryPlugin.addAgent(entryAgent);
            }
        }
    }

    private void addInitialPluginsToTreeList() {
        entryDevice.getPlugins().forEach(this::addPluginToList);

    }

    private void addAgentNodeToPluginNode(EntryPlugin entryPlugin, ContextTreeItem pluginNode) {
        for (EntryAgent agent : entryPlugin.getAgents()) {

            ContextTreeItem agentNode = new ContextTreeItem(agent, mProject.getProjectPath()); //It should not be ContextTreeItem
            agentNode.registerObserver(this);
            pluginNode.getChildren().add(agentNode);
        }
    }

    @FXML
    public void addNewItemToTableEvent(ActionEvent event) {
        if (!txtKey.getText().equals("") && !txtValue.getText().equals("")) {
            addNewItemToTable(txtKey.getText(), txtValue.getText());
        }
    }

    @FXML
    public void addDevice() {
        String deviceName = txtDeviceName.getText();
        try {
            tryToAddNewDevice(deviceName);
        } catch (ArrayIndexOutOfBoundsException e) {
            cmbExecutor.setStyle("-fx-outer-border: red");
        }
    }

    private void tryToAddNewDevice(String deviceName) {
        String className = activityClassesLongNames.get(cmbExecutor.getSelectionModel().getSelectedIndex());
        if (!deviceName.equals("")) {
            addNewDevice(deviceName, className);
        } else {
            cmbExecutor.setStyle(null);
            txtDeviceName.setStyle(FX_TEXT_BOX_BORDER_RED);
        }
    }

    private void addNewDevice(String deviceName, String className) {
        boolean added = projectConfigWrapper.addNewPlugin(deviceName, className);
        if (added) {
            PluginConfig plugin = mProject.getPluginConfig(deviceName);
            EntryPlugin entryPlugin = new EntryPlugin(plugin);
            entryDevice.addPlugin(entryPlugin);
            addPluginToList(entryPlugin);
            clearBorders();
        }
    }

    private void clearBorders() {
        txtDeviceName.setStyle(null);
        cmbExecutor.setStyle(null);
    }

    private void addPluginToList(EntryPlugin entryPlugin) {
        ContextTreeItem pluginNode = new ContextTreeItem(entryPlugin, mProject.getProjectPath());
        pluginNode.registerObserver(this);
        addAgentNodeToPluginNode(entryPlugin, pluginNode);
        devices.getChildren().add(pluginNode);
    }

    private void addNewItemToTable(String key, String value) {
        try {
            addTableConfigItem(key, value);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void addTableConfigItem(String key, String value) throws Exception {
        AbstractTreeEntry selectedItem = getSelectedTreeItem(false);
        if (selectedItem instanceof EntryAgent) {
            EntryAgent agent = (EntryAgent) selectedItem;
            addNewAgentToObservableTableAndSave(agent, key, value);
        }
        if (selectedItem instanceof EntryPlugin) {
            EntryPlugin plugin = (EntryPlugin) selectedItem;
            addNewDeviceToObservableTableAndSave(plugin, key, value);
        }
    }

    private void addNewAgentToObservableTableAndSave(EntryAgent agent, String key, String value) {
        AgentTableConfig dC = new AgentTableConfig(key, value, agent.getAgentConfig());
        data.add(dC);
        dC.saveEntry();
        saveConfig();
    }

    private void addNewDeviceToObservableTableAndSave(EntryPlugin plugin, String key, String value) {
        PluginTableConfig dC = new PluginTableConfig(key, value, plugin.getPluginConfig());
        data.add(dC);
        dC.saveEntry();
        saveConfig();
    }

    @FXML
    public void selectConfig(MouseEvent event) {
        try {
            processClickedTreeElement(event.getButton().toString().equals("SECONDARY"));
        } catch (Exception e) {
            System.out.println(e);
        }
    }

    @FXML
    public void handleLoadPluginCheckbox() {
        boolean loaded = false;
        if (chkLoadPlugin.isSelected()) {
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
        if (itemEntry instanceof EntryPlugin) {
            ((EntryPlugin) itemEntry).getPluginConfig().setLoad(loaded);
        } else if (itemEntry instanceof EntryAgent) {
            String pluginName = ((EntryAgent) itemEntry).getPluginName();
            mProject.getPluginConfig(pluginName).setLoad(loaded);
        }
        saveConfig();
    }

    private void processClickedTreeElement(boolean isRightClicked) throws Exception {
        AbstractTreeEntry itemEntry = null;
        itemEntry = getSelectedTreeItem(isRightClicked);
        if (itemEntry instanceof EntryDevice) {
            showAddDevice();
        }
        if (itemEntry instanceof EntryPlugin) {
            EntryPlugin entryPlugin = (EntryPlugin) itemEntry;
            showPluginDatainTable(entryPlugin);
            setLoadPluginCheckbox(entryPlugin.getPluginConfig());
            hideAddDevice();
            buildBasicPropertyBar(entryPlugin);

        }
        if (itemEntry instanceof EntryAgent) {
            EntryAgent entryAgent = (EntryAgent) itemEntry;
            showAgentDatainTable(entryAgent);
            String pluginName = entryAgent.getPluginName();
            setLoadPluginCheckbox(mProject.getPluginConfig(pluginName));
            hideAddDevice();
            disableBasicBar();
        }
    }

    private void buildBasicPropertyBar(EntryPlugin entryPlugin) throws ClassNotFoundException, NoSuchMethodException, InstantiationException, IllegalAccessException, InvocationTargetException, NotExportableInterface {
        exportableClassInitializer = new ExportableClassInitializer(mProject, entryPlugin.getPluginConfig());
        enableBasicBar();
        try {
            exportableClassInitializer.initializeClass();
            exportableProperties = exportableClassInitializer.getAsExportablePropertyClass().getExportableProperties();
            addExportableItemsToBasicBar();
        } catch (NotExportableInterface exception) {
            disableBasicBar();
        }
    }

    private void disableBasicBar() {
        basicBar.setVisible(false);
        advanceBar.setVisible(true);
        advancedButton.setDisable(true);
    }

    private void enableBasicBar() {
        basicBar.setVisible(true);
        advanceBar.setVisible(false);
        advancedButton.setDisable(false);
    }

    private void addExportableItemsToBasicBar() {
        ArrayList<ProjectProperty> propertyNames = new ArrayList<ProjectProperty>(exportableProperties.keySet());
        ObservableList obList = FXCollections.observableList(propertyNames);
        propertiesChooser.getItems().clear();
        propertiesChooser.setItems(obList);
    }

    private void setLoadPluginCheckbox(PluginConfig plugin) {
        chkLoadPlugin.setSelected(plugin.isMarkedtoLoad());
    }

    private AbstractTreeEntry getSelectedTreeItem(boolean isRightClicked) throws Exception {
        Object selectedItem = treeView.getSelectionModel().getSelectedItem();
        if (selectedItem == null) {
            throw new Exception("Not selected item");
        }
        AbstractTreeEntry itemEntry;
        if (selectedItem instanceof ContextTreeItem) {
            itemEntry = getContextItemSelected(isRightClicked);
        } else if (selectedItem instanceof BoxTreeItem) {
            itemEntry = (AbstractTreeEntry) ((BoxTreeItem) selectedItem).getValue();
        } else if (selectedItem instanceof AbstractTreeItem) {
            itemEntry = (AbstractTreeEntry) selectedItem;
        } else if (selectedItem instanceof TreeItem && ((TreeItem) selectedItem).getValue() instanceof EntryDevice) {
            itemEntry = (AbstractTreeEntry) ((TreeItem) selectedItem).getValue();
        } else {
            System.out.println("NO Datatype matched!");
            throw new Exception("Non datatype recognized");
        }
        return itemEntry;
    }

    private AbstractTreeEntry getContextItemSelected(boolean isRightClicked) {
        Object selectedItem = treeView.getSelectionModel().getSelectedItem();
        if (isRightClicked) {
            ((ContextTreeItem) selectedItem).getMenu();
            new ContextMenu(new MenuItem("HOLA"));

        }
        return ((ContextTreeItem) selectedItem).getEntryItem();
    }

    private void showPluginDatainTable(EntryPlugin entryPlugin) {
        showPluginTable(entryPlugin);
        setClassNameLabel(entryPlugin.getPluginClassName());
    }

    private void showAgentDatainTable(EntryAgent entryAgent) {
        showAgentTable(entryAgent);
        String pluginName = entryAgent.getPluginName();
        setClassNameLabel(mProject.getPluginConfig(pluginName).getClassName());
    }

    private void showAddDevice() {
        editDevice.setVisible(false);
        InfoVBox.setVisible(false);
        addDevice.setVisible(true);
    }

    private void hideAddDevice() {
        editDevice.setVisible(true);
        InfoVBox.setVisible(true);
        addDevice.setVisible(false);
    }

    private void setClassNameLabel(String className) {
        lblClassName.setText("Class: " + className);
    }

    private void showPluginTable(EntryPlugin plugin) {
        pluginsTable.setEditable(true);
        getPluginData(plugin.getPluginConfig());
    }

    private void showAgentTable(EntryAgent agent) {
        pluginsTable.setEditable(true);
        getAgentData(agent.getAgentConfig());
    }

    private void getPluginData(PluginConfig plugin) {
        data.clear();
        for (ConfigFeature feat : plugin.getEntryList()) {
            PluginTableConfig tFeat = new PluginTableConfig(feat.getKey(), feat.getValue(), plugin);
            data.add(tFeat);
        }
        populatePluginTable(plugin.getPluginName());
    }

    private void getAgentData(AgentConfig agent) {
        data.clear();
        for (ConfigFeature feat : agent.getEntryList()) {
            AgentTableConfig tFeat = new AgentTableConfig(feat.getKey(), feat.getValue(), agent);
            data.add(tFeat);
        }
        populatePluginTable(agent.getDeviceName());
    }

    private void populatePluginTable(String pluginName) {
        ObservableList<String> pluginList = FXCollections.observableArrayList();
        setEditableValueCell(pluginName);
        setEditablKeyeCell(pluginName);
        pluginsTable.setItems(data);
    }

    private void setEditablKeyeCell(String pluginName) {
        keyColumn.setCellFactory(TextFieldTableCell.forTableColumn());

        keyColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
            @Override
            public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                TableConfig dataConfig = t.getRowValue();
                if (t.getOldValue() != t.getNewValue()) {
                    removeOldProperty(dataConfig, t.getOldValue());
                }

                ((TableConfig) t.getTableView().getItems().get(
                        t.getTablePosition().getRow())).setKey(t.getNewValue());

                saveEditedTableEntry(dataConfig);

            }
        }
        );
        keyColumn.setCellValueFactory(new PropertyValueFactory("key"));
    }

    private void setEditableValueCell(String pluginName) {
        valueColumn.setCellFactory(TextFieldTableCell.forTableColumn());
        valueColumn.setOnEditCommit(
                new EventHandler<TableColumn.CellEditEvent<TableConfig, String>>() {
            @Override
            public void handle(TableColumn.CellEditEvent<TableConfig, String> t) {
                ((TableConfig) t.getTableView().getItems().get(
                        t.getTablePosition().getRow())).setValue(t.getNewValue());
                TableConfig dataConfig = t.getRowValue();
                saveEditedTableEntry(dataConfig);
            }
        }
        );
        valueColumn.setCellValueFactory(new PropertyValueFactory("value"));
    }

    private void saveEditedTableEntry(TableConfig dataConfig) {
        if (dataConfig instanceof AgentTableConfig) {
            ((AgentTableConfig) dataConfig).saveEntry();
        }
        if (dataConfig instanceof PluginTableConfig) {
            ((PluginTableConfig) dataConfig).saveEntry();
        }
        saveConfig();
    }

    private void removeOldProperty(TableConfig dataConfig, String oldValue) {
        if (dataConfig instanceof AgentTableConfig) {
            ((AgentTableConfig) dataConfig).removeProperty(oldValue);
        }
        if (dataConfig instanceof PluginTableConfig) {
            ((PluginTableConfig) dataConfig).removeProperty(oldValue);
        }
    }

    public void saveConfig() {
        projectConfigWrapper.saveConfig();
    }

    @Override
    public void update(NotificationObject object) {
        if (object instanceof ContextEvent) {//This called when a new agent is added via context menu
            String agent = ((ContextEvent) object).getContextName();
            String pluginName = ((ContextEvent) object).getPluginName();
            EntryAgent agentEntry = (EntryAgent) ((ContextEvent) object).getTreeEntry();

            projectConfigWrapper.addNewAgent(agent, pluginName);
            ContextTreeItem contextItem = (ContextTreeItem) agentEntry.getContextTreeItem();
            if (contextItem != null) {
                contextItem.registerObserver(this);
            }
            agentEntry.setAgentConfig(mProject.getAgentConfig(agent));
        } else if (object instanceof CellEvent) {//We change either the name of the agent or device
            changeItemName((CellEvent) object);
        } else if (object instanceof DeleteContextEventAgent) {
            removeSelectedItem();
            deleteAgent(((DeleteContextEventAgent) object).getTreeEntry());
            saveConfig();
        } else if (object instanceof DeleteContextEventPlugin) {
            removeSelectedItem();
            deletePlugin(((DeleteContextEventPlugin) object).getTreeEntry());
            saveConfig();
        }
    }

    private void removeSelectedItem() {
        TreeItem selectedItem = treeView.getSelectionModel().getSelectedItem();
        treeView.getSelectionModel().getSelectedItem().getParent().getChildren().remove(selectedItem);
    }

    private void deletePlugin(AbstractTreeEntry treeEntry) {
        EntryPlugin entry = (EntryPlugin) treeEntry;
        mProject.deletePlugin(entry.getPluginConfig());
    }

    private void deleteAgent(AbstractTreeEntry treeEntry) {
        EntryAgent entry = (EntryAgent) treeEntry;
        mProject.deleteAgent(entry.getAgentConfig());
    }

    private void changeItemName(CellEvent event) {
        String oldValue = ((CellEvent) event).getOldValue();
        String newValue = ((CellEvent) event).getNewValue();
        AbstractTreeEntry entry = ((CellEvent) event).getTreeEntry();
        if (entry instanceof EntryPlugin) {
            changePluginName((EntryPlugin) entry, oldValue, newValue);

        }
        if (entry instanceof EntryAgent) {
            changeAgentName((EntryAgent) entry, oldValue, newValue);
        }
    }

    private void changePluginName(EntryPlugin plugin, String oldValue, String newValue) {
        try {
            projectConfigWrapper.changePluginName(plugin.getPluginConfig(), oldValue, newValue);
            plugin.setPluginConfig(mProject.getPluginConfig(newValue));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void changeAgentName(EntryAgent agent, String oldValue, String newValue) {
        try {
            projectConfigWrapper.changeAgentName(oldValue, newValue);
            agent.setAgentConfig(mProject.getAgentConfig(newValue));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
