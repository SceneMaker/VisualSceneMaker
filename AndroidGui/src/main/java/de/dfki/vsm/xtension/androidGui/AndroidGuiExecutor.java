package de.dfki.vsm.xtension.androidGui;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.AbstractValue;
import de.dfki.vsm.runtime.project.RunTimeProject;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;

public class AndroidGuiExecutor extends ActivityExecutor {

    private Map<String, AndroidActivity> activityMap = new HashMap<>();
    private Consumer<String> launcher;

    public AndroidGuiExecutor(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public String marker(long id) {
        return null;
    }

    @Override
    public void execute(AbstractActivity activity) {
        final String actorName = activity.getActor();
        final String id = activity.get("id");
        if (activityMap.containsKey(actorName)) {
            AndroidActivity view = activityMap.get(actorName);
            switch (activity.getName()) {
                case "setval": {
                    System.out.println("setval");
                    final String value = activity.get("value");
                    if (view.getLabels().containsKey(id)) {
                        view.getLabels().get(id).accept(value);
                    } else if (view.getEditFields().containsKey(id)) {
                        view.getEditFields().get(id).setText.accept(value);
                    }
                    break;
                }
                case "setvar": {
                    AbstractValue abstractValue = mProject.getValueOf(activity.get("value"));
                    final String value = abstractValue.getType() == AbstractValue.Type.STRING ? (String) abstractValue.getValue() : abstractValue.getConcreteSyntax();
                    if (view.getLabels().containsKey(id)) {
                        view.getLabels().get(id).accept(value);
                    } else if (view.getEditFields().containsKey(id)) {
                        view.getEditFields().get(id).setText.accept(value);
                    }
                    break;
                }
                case "show": {
                    launcher.accept(activity.get("name"));
                    try {
                        // Wait for Android to show activity, value may be tweaked
                        Thread.sleep(100);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    break;
                }
            }
        }
    }

    @Override
    public void launch() {


    }

    @Override
    public void unload() {

    }

    public AndroidActivity getActivity(String name) {
        AndroidActivity androidActivity = new AndroidActivity(name, mProject);
        activityMap.put(name, androidActivity);
        return androidActivity;
    }

    public void bindLauncher(Consumer<String> stringConsumer) {
        if (launcher == null)
            this.launcher = stringConsumer;
    }

    public void deleteActivity(String simpleName) {
        activityMap.remove(simpleName);
    }
}
