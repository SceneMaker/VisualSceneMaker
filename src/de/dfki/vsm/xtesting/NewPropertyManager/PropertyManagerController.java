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
import javafx.util.Callback;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.net.URL;
import java.util.*;

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
    private TreeItem<AbstractTreeEntry> devices;
    private HashMap<String, PluginConfig> plugins= new HashMap<>();
    private EntryDevice entryDevice;
    private ObservableList<TableConfig> data = FXCollections.observableArrayList();


    public PropertyManagerController(RunTimeProject project){
        super();
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
        treeView.setCellFactory(new Callback<TreeView<AbstractTreeEntry>,TreeCell<AbstractTreeEntry>>(){
            @Override
            public TreeCell<AbstractTreeEntry> call(TreeView<AbstractTreeEntry> param) {
                TreeCellImpl treeCell = new TreeCellImpl();
                treeCell.registerObserver(controller);
                return treeCell;
            }
        });
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
        for (EntryPlugin entryPlugin: entryDevice.getPlugins() ) {
            ContextTreeItem pluginNode = new ContextTreeItem(entryPlugin);
            pluginNode.registerObserver(this);
            addAgentNodeToPluginNode(entryPlugin, pluginNode);
            devices.getChildren().add(pluginNode);
        }

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

    private void processClickedTreeElement(boolean isRightClicked) throws Exception {
        AbstractTreeEntry itemEntry = null;
        itemEntry = getSelectedTreeItem(isRightClicked);
        if(itemEntry instanceof EntryDevice){
            showAddDevice();
        }
        if(itemEntry instanceof EntryPlugin){
            EntryPlugin entryPlugin = (EntryPlugin) itemEntry;
            showPluginDatainTable(entryPlugin);
        }
        if(itemEntry instanceof EntryAgent){
            EntryAgent entryAgent= (EntryAgent) itemEntry;
            showAgentDatainTable(entryAgent);
        }
    }

    private AbstractTreeEntry getSelectedTreeItem(boolean isRightClicked) throws Exception {
        Object selectedItem = treeView.getSelectionModel().getSelectedItem();
        if(selectedItem == null){
           throw  new Exception("Not selected item");
        }
        AbstractTreeEntry itemEntry;
        if(selectedItem instanceof  ContextTreeItem){
            itemEntry = getContextItemSelected(isRightClicked);
        }else if(selectedItem instanceof AbstractTreeItem){
            itemEntry = (AbstractTreeEntry) selectedItem;
        }else {
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
        setClassNameLabel("Test");
    }


    private void showAddDevice(){
        System.out.println("To implement!!");
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
        //TODO: Remove later
        File f = new File(mProject.getProjectPath());
        mProject.write(f);
    }

    private String changeXMLAttributeNode(String xml, String tagName, String attr, String oldValue, String newValue){
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder docBuilder = null;
        Document doc;
        try {
            docBuilder = docFactory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            e.printStackTrace();
        }
        try {
            doc = docBuilder.parse(new InputSource(new ByteArrayInputStream(xml.getBytes("utf-8"))));
        } catch (SAXException e) {
            e.printStackTrace();
            return xml;
        } catch (IOException e) {
            e.printStackTrace();
            return xml;
        }
        //Node firstChild = doc.getFirstChild();
        Node tag = doc.getElementsByTagName(tagName).item(0);
        // update node attribute
        NamedNodeMap attrs = tag.getAttributes();
        Node nodeAttr = attrs.getNamedItem(attr);
        if(nodeAttr!= null && nodeAttr.getNodeValue().equals(oldValue)) {
            nodeAttr.setTextContent(newValue);
        }

        return getStringFromDoc(doc);

    }

    public String getStringFromDoc(org.w3c.dom.Document doc)    {
        try {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            StreamResult result = new StreamResult(new StringWriter());
            DOMSource source = new DOMSource(doc);
            transformer.transform(source, result);
            return result.getWriter().toString();
        } catch(TransformerException ex) {
            ex.printStackTrace();
            return null;
        }
    }

    @Override
    public void update(NotificationObject object) {
        if(object instanceof ContextEvent){//This called when a new agent is added via context menu
            String agent = ((ContextEvent) object).getContextName();
            String pluginName = ((ContextEvent) object).getPluginName();
            AbstractTreeEntry entry = ((ContextEvent) object).getTreeEntry();
            String newXMLAgent = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                    + "<Project name=\"" + mProject.getProjectName() + "\">"
                    + "<Agents>"
                    + "    <Agent name=\"" + agent + "\" device=\"" + pluginName + "\">"
                    + "    </Agent>"
                    + " </Agents>"
                    + "</Project>";
            boolean res = mProject.parseProjectConfigFromString(newXMLAgent);
            saveConfig();
        }
        else if(object instanceof CellEvent){//We change either the name of the agent or device
            String oldValue = ((CellEvent) object).getOldValue();
            String newValue = ((CellEvent) object).getNewValue();
            TreeItem selected = treeView.getSelectionModel().getSelectedItem();
            AbstractTreeEntry entry = ((CellEvent) object).getTreeEntry();
            if(selected !=null && (selected.getParent() != null && selected.getParent().getValue().equals("Devices"))){//Device updated

               PluginConfig pluginConfig = mProject.getPluginConfig(oldValue);
                Iterator<PluginConfig> itPlug = mProject.getProjectConfig().getPluginConfigList().iterator();
                ArrayList<String> pluginsToAdd = new ArrayList<>();
                ArrayList<String> agentsToAdd = new ArrayList<>();
                while (itPlug.hasNext()){
                    PluginConfig plugin = itPlug.next();
                    if(plugin.getPluginName().equals(oldValue)){
                        String pluginXML =   "<Project name=\"" + mProject.getProjectName() + "\">"
                                + "<Plugins>"
                                + plugin.toString()
                                + "</Plugins>"
                                + "</Project>";
                        String xml = changeXMLAttributeNode(pluginXML, "Plugin", "name", oldValue, newValue);
                        if(xml != null && !xml.equals("")){
                            itPlug.remove();
                            plugins.remove(oldValue);

                        }
                        pluginsToAdd.add(xml);
                    }
                }
                for (String xml: pluginsToAdd ) {
                    boolean res = mProject.parseProjectConfigFromString(xml);
                    if(res) {
                        ArrayList plugs =  mProject.getProjectConfig().getPluginConfigList();
                        plugins.put(newValue, (PluginConfig) plugs.get(plugs.size()-1));
                    }

                }

                //Change all the agents too
                Iterator<AgentConfig> itAgent = mProject.getProjectConfig().getAgentConfigList().iterator();
                while (itAgent.hasNext()){
                    AgentConfig agent = itAgent.next();
                    if(agent.getDeviceName().equals(oldValue) ){
                        String agentXML = "<Project name=\"" + mProject.getProjectName() + "\">"
                                + "<Agents>"
                                + agent.toString()
                                + "</Agents>"
                                + "</Project>";
                        String xml = changeXMLAttributeNode(agentXML, "Agent", "device", oldValue, newValue);
                        if(xml != null && !xml.equals("")){
                            itAgent.remove();

                        }
                        agentsToAdd.add(xml);
                    }
                }

                for (String xml: agentsToAdd ) {
                    boolean res = mProject.parseProjectConfigFromString(xml);
                }

            }
            else if(selected !=null && (selected.getParent() != null)) {
                TreeItem selectedPlugin = treeView.getSelectionModel().getSelectedItem().getParent();
                ArrayList<String> agentsToAdd = new ArrayList<>();
                Iterator<AgentConfig> itAgent = mProject.getProjectConfig().getAgentConfigList().iterator();
                while (itAgent.hasNext()){
                    AgentConfig agent = itAgent.next();
                    if(agent.getDeviceName().equals(selectedPlugin.getValue()) && agent.getAgentName().equals(oldValue)){
                        String agentXML = "<Project name=\"" + mProject.getProjectName() + "\">"
                                + "<Agents>"
                                + agent.toString()
                                + "</Agents>"
                                + "</Project>";
                        String xml = changeXMLAttributeNode(agentXML, "Agent", "name", oldValue, newValue);
                        if(xml != null && !xml.equals("")){
                            itAgent.remove();
                        }
                        agentsToAdd.add(xml);
                    }
                }

                for (String xml: agentsToAdd ) {
                    boolean res = mProject.parseProjectConfigFromString(xml);
                }

            }
            saveConfig();
        }
    }








}
