package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.reflections.Reflections;

import java.lang.reflect.Modifier;
import java.util.ArrayList;

/**
 * Created by alvaro on 6/3/16.
 * Refactor: Lenny 12/sept/2020: replace jar parsing with reflection.
 */
public class ExtensionsFromJar {
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private final ArrayList<String> mScenePlayersShortNames = new ArrayList<>();
    private final ArrayList<String> mScenePlayersLongNames = new ArrayList<>();
    private final String packageName;
    private final boolean nonSelectedFirst;

    public ExtensionsFromJar(String packName, boolean pAddNonSelectedFirst) {
        packageName = packName;
        nonSelectedFirst = pAddNonSelectedFirst;
    }

    public ExtensionsFromJar(String packName) {
        packageName = packName;
        nonSelectedFirst = true;
    }

    public void loadExtensions() {
        new Reflections(packageName)
                .getSubTypesOf(RunTimePlugin.class).stream()
                .filter(aClass -> !Modifier.isAbstract(aClass.getModifiers()) &&
                        !Modifier.isInterface(aClass.getModifiers()))
                .forEach(aClass -> {
                    mScenePlayersLongNames.add(aClass.getCanonicalName());
                    mScenePlayersShortNames.add(aClass.getSimpleName());
                });

    }

    public ArrayList<String> getActivitiesShortNames() {
        return mScenePlayersShortNames;
    }

    public ArrayList<String> getActivitiesLongName() {
        return mScenePlayersLongNames;
    }

    // Added PG 14.8.2020, refactor by Lenny
    /*
     * Checks if a specific class is a subclass of the ActivityExecutor class
     */
    public boolean isClassAnActivityExecutor(String className) {
        return new Reflections(packageName).getSubTypesOf(ActivityExecutor.class).stream()
                .anyMatch(aClass -> aClass.getCanonicalName().equals(className));
    }

}
