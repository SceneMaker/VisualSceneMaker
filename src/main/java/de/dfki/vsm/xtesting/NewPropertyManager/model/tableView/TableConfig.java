package de.dfki.vsm.xtesting.NewPropertyManager.model.tableView;

/**
 * Created by alvaro on 4/25/16.
 */
public class TableConfig {

    protected String value;
    protected String key;



    public TableConfig(){

    }

    public TableConfig(String pKey, String pValue){
        value = pValue;
        key = pKey;
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


}
