package de.dfki.vsm.xtesting.NewPropertyManager.util;

import de.dfki.vsm.util.log.LOGConsoleLogger;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Created by alvaro on 6/3/16.
 */
public class ExtensionsFromJar {
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private ArrayList<String> mScenePlayersShortNames = new ArrayList<>();
    private ArrayList<String> mScenePlayersLongNames = new ArrayList<>();
    private String packageName;
    private boolean nonSelectedFirst;

    public ExtensionsFromJar(String packName, boolean pAddNonSelectedFirst) {
        packageName = packName;
        nonSelectedFirst = pAddNonSelectedFirst;
    }

    public ExtensionsFromJar(String packName) {
        packageName = packName;
        nonSelectedFirst = true;
    }

    public ArrayList getActivitiesShortNames() {
        return mScenePlayersShortNames;
    }

    public ArrayList getActivitiesLongName() {
        return mScenePlayersLongNames;
    }

    public void loadClass() {
        try {
            if (mScenePlayersShortNames.size() <= 0) {
                addNoSelectedAtFirst();
                getClassNamesFromPackage(packageName);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void getClassNamesFromPackage(String packageName) throws IOException {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL packageURL;
        packageName = packageName.replace(".", "/");
        packageURL = classLoader.getResource(packageName);
        if (packageURL.getProtocol().equals("jar")) {
            packageIsJar(packageURL);
        }
    }

    // Added PG 14.8.2020
    /*
     * Checks if a specific class is a subclass of the ActivityExecutor class
     */
    public boolean isClassAnActivityExecutor(String className) {
        boolean isAE = false;
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL packageURL;
        packageName = packageName.replace(".", "/");
        packageURL = classLoader.getResource(packageName);
        if (packageURL.getProtocol().equals("jar")) {
            String jarFileName;
            // build jar file name, then loop through zipped entries
            jarFileName = URLDecoder.decode(packageURL.getFile(), StandardCharsets.UTF_8);
            jarFileName = jarFileName.substring(5, jarFileName.indexOf("!"));

            JarFile jf;
            Enumeration<JarEntry> jarEntries = null;
            try {
                jf = new JarFile(jarFileName);
                jarEntries = jf.entries();

                while (jarEntries.hasMoreElements()) {
                    String entryName;
                    entryName = jarEntries.nextElement().getName();
                    try {
                        if ((packageName.length() == 0 || (entryName.startsWith(packageName) && entryName.length() > packageName.length() + 5))) {
                            entryName = entryName.replace("/", ".");

                            if (entryName.contains(className)) {
                                String cleanClassName = entryName.substring(0, entryName.lastIndexOf('.'));

                                if (className.equalsIgnoreCase(cleanClassName)) {
                                    Class classEntry = Class.forName(cleanClassName);
                                    Class superClass = classEntry.getSuperclass();

                                    isAE = superClass != null && (superClass.getSimpleName().equals("ActivityExecutor"));
                                }
                            }
                        }
                    } catch (ClassNotFoundException e) {
                        mLogger.failure("Class " + entryName + " not found in SceneMaker jar file.");
                    }
                }
            } catch (IOException e) {
                mLogger.failure("Error parsing SceneMaker jar file.");
                isAE = false;
            }
        }

        return isAE;
    }

    private void addNoSelectedAtFirst() {
        if (nonSelectedFirst) {
            mScenePlayersShortNames.add("Non selected");
        }
    }

    private void packageIsJar(URL packageURL) throws UnsupportedEncodingException {
        String jarFileName;
        // build jar file name, then loop through zipped entries
        jarFileName = URLDecoder.decode(packageURL.getFile(), StandardCharsets.UTF_8);
        jarFileName = jarFileName.substring(5, jarFileName.indexOf("!"));
        mLogger.message("Using " + jarFileName);
        parseJar(jarFileName);
    }

    private void parseJar(String jarFileName) {
        JarFile jf;
        Enumeration<JarEntry> jarEntries = null;
        try {
            jf = new JarFile(jarFileName);
            jarEntries = jf.entries();
            ExtractExtensions(jarEntries);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void ExtractExtensions(Enumeration<JarEntry> jarEntries) {
        while (jarEntries.hasMoreElements()) {
            tryToAddActivityExecutor(jarEntries);
        }
    }

    private void tryToAddActivityExecutor(Enumeration<JarEntry> jarEntries) {
        String entryName;
        entryName = jarEntries.nextElement().getName();
        try {
            if (isActivityExecutor(entryName)) {
                addActivityExecutor(entryName);
            }
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    private boolean isActivityExecutor(String entryName) throws ClassNotFoundException {
        entryName = entryName.replace("/", ".");
        boolean belongsToPackage = (packageName.length() == 0 || (entryName.startsWith(packageName) && entryName.length() > packageName.length() + 5));
        if (belongsToPackage) {
            String fullClassName = entryName.replace("/", ".");
            String className = fullClassName.substring(0, entryName.lastIndexOf('.'));
            Class classEntry = Class.forName(className);
            Class superClass = classEntry.getSuperclass();
            // 13.8.2020 - PG added RuntimePlugins
            return ((superClass != null && (superClass.getSimpleName().equals("ActivityExecutor"))) || (superClass != null && (superClass.getSimpleName().equals("RunTimePlugin"))));
        }
        return false;
    }

    private void addActivityExecutor(String entryName) throws ClassNotFoundException {
        entryName = entryName.replace("/", ".");
        String className = entryName.substring(0, entryName.lastIndexOf('.'));
        mScenePlayersLongNames.add(className);
        mScenePlayersShortNames.add(className.substring(className.lastIndexOf('.') + 1));
    }
}
