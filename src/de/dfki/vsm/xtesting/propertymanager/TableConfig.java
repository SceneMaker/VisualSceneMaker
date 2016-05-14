package de.dfki.vsm.xtesting.propertymanager;

/**
 * Created by alvaro on 4/25/16.
 */
public class TableConfig {

    private String value;
    private String key;
    private String plugin;
    private String agent;
    private String device;


    public TableConfig(){

    }

    TableConfig(String pKey, String pValue, String pPlugin){
        value = pValue;
        key = pKey;
        plugin = pPlugin;
    }

    TableConfig(String pKey, String pValue){
        value = pValue;
        key = pKey;
    }

    TableConfig(String pKey, String pValue, String pDevice, String pAgent){
        value = pValue;
        key = pKey;
        agent = pAgent;
        device = pDevice;
    }
    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public String getPlugin() {
        return plugin;
    }

    public void setPlugin(String plugin) {
        this.plugin = plugin;
    }


    public String getAgent() {
        return agent;
    }

    public void setAgent(String agent) {
        this.agent = agent;
    }

    public String getDevice() {
        return device;
    }

    public void setDevice(String device) {
        this.device = device;
    }
}
