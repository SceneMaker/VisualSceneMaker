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
    private final Consumer<String> uiEventPublisher;

    public AndroidActivity(String name, RunTimeProject mProject) {
        this(name, mProject, null);
    }

    public AndroidActivity(String name, RunTimeProject mProject, Consumer<String> uiEventPublisher) {
        this.uiEventPublisher = uiEventPublisher;
        AgentConfig agentConfig = mProject.getAgentConfig(name);
        if (agentConfig == null) {
            throw new NullPointerException("Android activity has no corresponding agent");
        }
        String textFields = agentConfig.getProperty("textFields");
        if (textFields != null) {
            String[] ids = textFields.split(",");
            for (String textField : ids) {
                final String id = textField == null ? "" : textField.trim();
                if (!id.isEmpty()) {
                    textMap.put(id, null);
                }
            }
        }

        String editFields = agentConfig.getProperty("editFields");
        if (editFields != null) {
            String[] ids = editFields.split(",");
            for (String editField : ids) {
                final String id = editField == null ? "" : editField.trim();
                if (id.isEmpty()) {
                    continue;
                }
                editMap.put(id, new AndroidTextField(new Consumer<String>() {
                            @Override
                            public void accept(String s) {
                                mProject.setVariable(id, new StringValue(s));
                            }
                        }, null)
                );
            }
        }

        String buttons = agentConfig.getProperty("buttons");
        if (buttons != null) {
            String[] ids = buttons.split(",");
            for (String button : ids) {
                final String id = button == null ? "" : button.trim();
                if (id.isEmpty()) {
                    continue;
                }
                buttonMap.put(id, new AndroidButton(new Consumer<Void>() {
                    @Override
                    public void accept(Void aVoid) {
                        mProject.setVariable(id, new StringValue(id));
                        if (AndroidActivity.this.uiEventPublisher != null) {
                            AndroidActivity.this.uiEventPublisher.accept(id);
                        }
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
