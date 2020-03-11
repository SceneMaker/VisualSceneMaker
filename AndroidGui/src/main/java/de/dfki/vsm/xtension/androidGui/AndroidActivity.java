package de.dfki.vsm.xtension.androidGui;

import de.dfki.vsm.model.project.AgentConfig;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class AndroidActivity {
    private Map<String, AndroidLabel> textMap = new HashMap<>();
    private Map<String, AndroidTextField> editMap = new HashMap<>();
    private Map<String, AndroidButton> buttonMap = new HashMap<>();
    private boolean initialized;

    public AndroidActivity(String name, RunTimeProject mProject) {
        AgentConfig agentConfig = mProject.getAgentConfig(name);
        if (agentConfig == null) {
            throw new NullPointerException("Android activity has no corresponding agent");
        }
        String textFields = agentConfig.getProperty("textFields");
        if (textFields != null) {
            String[] ids = textFields.split(",");
            for (String textField : ids) {
                textMap.put(textField, null);
            }
        }

        String editFields = agentConfig.getProperty("editFields");
        if (editFields != null) {
            String[] ids = editFields.split(",");
            for (String editField : ids) {
                System.out.println(editField);
                editMap.put(editField, new AndroidTextField(new Consumer<String>() {
                            @Override
                            public void accept(String s) {
                                mProject.setVariable(editField, new StringValue(s));
                            }
                        }, null)
                );
            }
        }

        String buttons = agentConfig.getProperty("buttons");
        if (buttons != null) {
            String[] ids = buttons.split(",");
            for (String button : ids) {
                buttonMap.put(button, new AndroidButton(new Consumer<Void>() {
                    @Override
                    public void accept(Void aVoid) {
                        mProject.setVariable(button, new StringValue(button));
                    }
                }, null));
            }
        }
    }

    public Map<String, AndroidLabel> getLabels() {
        return textMap;
    }

    public Map<String, AndroidButton> getButtons() {
        return buttonMap;
    }

    public Map<String, AndroidTextField> getEditFields() {
        return editMap;
    }
}
