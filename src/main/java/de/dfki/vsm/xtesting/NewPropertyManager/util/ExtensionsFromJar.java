package de.dfki.vsm.xtesting.NewPropertyManager.util;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
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

    private ArrayList<String> mScenePlayersShortNames = new ArrayList<>();
    private ArrayList<String> mScenePlayersLongNames = new ArrayList<>();
    private String packageName;
    private boolean nonSelectedFirst;

    public ExtensionsFromJar(String packName, boolean pAddNonSelectedFirst) {
        packageName = packName;
        nonSelectedFirst = pAddNonSelectedFirst;
    }

    public ExtensionsFromJar(String packName) {
        this(packName, true);
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

                if (nonSelectedFirst) {
                    mScenePlayersShortNames.add("Non selected");
                }

                getClassNamesFromPackage(packageName);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void getClassNamesFromPackage(String packageName) throws IOException, URISyntaxException {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        packageName = packageName.replace(".", "/");
        // URL packageURL = classLoader.getResource(packageName);

        Enumeration<URL> resources = classLoader.getResources(packageName);

        while (resources.hasMoreElements()) {
            URL packageURL = resources.nextElement();


            // https://stackoverflow.com/a/7461653/2010713
            if (packageURL.getProtocol().equals("jar")) {
                parseJar(packageURL);
            }

        }
    }

    /**
     * Scan all classes (.class files) in the given Jar and add them to the
     * class-level list of class names if they are subclasses of Activity Executor.
     *
     * @param packageURL
     */
    private void parseJar(URL packageURL) {
        // build jar file name, then loop through zipped entries
        String jarFileName = URLDecoder.decode(packageURL.getFile(), StandardCharsets.UTF_8);
        jarFileName = jarFileName.substring(5, jarFileName.indexOf("!"));
        System.out.println(">" + jarFileName);

        try {
            JarFile jf = new JarFile(jarFileName);
            Enumeration<JarEntry> jarEntries = jf.entries();

            // Look for Activity Executors in all entries
            while (jarEntries.hasMoreElements()) {
                String entryName = jarEntries.nextElement().getName();
                try {
                    if (isActivityExecutor(entryName)) {
                        addActivityExecutor(entryName);
                    }
                } catch (Exception e) {
                    System.out.println("Exception checking entry "+entryName+ " --> " + e);
                }
            }

        } catch (Exception e) {
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
            return (superClass != null && (superClass.getSimpleName().equals("ActivityExecutor")));
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
