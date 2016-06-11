package de.dfki.vsm.xtesting.NewPropertyManager.util;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Created by alvaro on 6/3/16.
 */
public class ExtensionsFromJar {

    private  ArrayList<String> mScenePlayersShortNames = new ArrayList<>();
    private  ArrayList<String> mScenePlayersLongNames = new ArrayList<>();
    private  String packageName;
    private  boolean nonSelectedFirst;
    public ExtensionsFromJar(String packName, boolean pAddNonSelectedFirst){
        packageName = packName;
        nonSelectedFirst = pAddNonSelectedFirst;
    }

    public ExtensionsFromJar(String packName){
        packageName = packName;
        nonSelectedFirst = true;
    }
    public ArrayList getActivitiesShortNames(){
        return mScenePlayersShortNames;
    }

    public ArrayList getActivitiesLongName(){
        return mScenePlayersLongNames;
    }

    public  void loadClass() {
        try{
            if (mScenePlayersShortNames.size() <= 0) {
                addNoSelectedAtFirst();
                getClassNamesFromPackage(packageName);
            }
        } catch (IOException e)
        {
            e.printStackTrace();
        }
    }

    public  void getClassNamesFromPackage(String packageName) throws IOException {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        URL packageURL;
        ArrayList<String> names = new ArrayList<String>();
        packageName = packageName.replace(".", "/");
        packageURL = classLoader.getResource(packageName);
        if (packageURL.getProtocol().equals("jar")) {
            packageIsJar(packageURL);
        }
    }

    private void addNoSelectedAtFirst(){
        if(nonSelectedFirst){
            mScenePlayersShortNames.add("Non selected");
        }
    }

    private void packageIsJar(URL packageURL) throws UnsupportedEncodingException {
        String jarFileName;
        // build jar file name, then loop through zipped entries
        jarFileName = URLDecoder.decode(packageURL.getFile(), "UTF-8");
        jarFileName = jarFileName.substring(5, jarFileName.indexOf("!"));
        System.out.println(">" + jarFileName);
        parseJar(jarFileName);
    }

    private  void parseJar(String jarFileName){
        JarFile jf;
        Enumeration<JarEntry> jarEntries = null;
        try {
            jf = new JarFile(jarFileName);
            jarEntries = jf.entries();
            ExtractExtensions( jarEntries);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private  void ExtractExtensions( Enumeration<JarEntry> jarEntries) {
        while (jarEntries.hasMoreElements())
        {
            tryToAddActivityExecutor(jarEntries);
        }
    }

    private void tryToAddActivityExecutor(Enumeration<JarEntry> jarEntries) {
        String entryName;
        entryName = jarEntries.nextElement().getName();
        try {
            if (isActivityExecutor(entryName)){
                addActivityExecutor(entryName);
            }
        } catch (ClassNotFoundException e) {
            return;

        }

    }

    private boolean isActivityExecutor(String entryName) throws ClassNotFoundException {
        entryName = entryName.replace("/", ".");
        boolean belongsToPackage = (packageName.length() == 0 || (entryName.startsWith(packageName) && entryName.length() > packageName.length() + 5));
        if(belongsToPackage) {
            String fullClassName = entryName.replace("/", ".");
            String className = fullClassName.substring(0, entryName.lastIndexOf('.'));
            Class classEntry = Class.forName(className);
            Class superClass = classEntry.getSuperclass();
            return  (superClass != null && superClass.getSimpleName().equals("ActivityExecutor"));
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
