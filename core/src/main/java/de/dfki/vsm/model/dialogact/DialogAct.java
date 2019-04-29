package de.dfki.vsm.model.dialogact;

//~--- JDK imports ------------------------------------------------------------
import java.util.List;
import java.util.Map;

/**
 *
 * @author Sergio Soto
 */
public class DialogAct {

    private final String mPhase;
    private final String mName;
    private Map<String, List<String>> mAttributeValueMap;

    public DialogAct(String name, String phase, Map<String, List<String>> attributeValueMap) {
        mName = name;
        mPhase = phase;
        mAttributeValueMap = attributeValueMap;
    }

    public String getName() {
        return mName;
    }

    public String getPhase() {
        return mPhase;
    }
}
