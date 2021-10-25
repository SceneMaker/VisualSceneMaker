/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.charamel.xml.util;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.charamel.xml.command.object.action.CharamelActObject;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;

/**
 * @author Patrick Gebhard
 */
public class
CharamelActionLoader {

    public final static String sCHARAMELCMDPATH = "de.dfki.vsm.xtension.charamel.xml.command.object.action.charamel";
    private static CharamelActionLoader sInstance = null;
    private volatile static long sID = 0;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private CharamelActionLoader() {
    }

    public static CharamelActionLoader getInstance() {
        if (sInstance == null) {
            sInstance = new CharamelActionLoader();
        }

        return sInstance;
    }

    public synchronized String getNextID() {
        sID++;
        return "tw" + sID;
    }

    private String getCharamelCommandClasspath(String cmd) {
        String classPath = "";

        classPath = sCHARAMELCMDPATH + "." + cmd;

        try {
            Class.forName(classPath);
        } catch (ClassNotFoundException ex) {
            mLogger.failure("Wrong classpath for Charamel Action " + cmd);
        }

        return classPath;
    }

    public CharamelActObject buildCharamelAnimation(String cmd, LinkedList value1, String value2, String value3) {
        CharamelActObject a = null;

        String cp = getCharamelCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 3) {
                    if (params[0].getSimpleName().equalsIgnoreCase("linkedlist") && params[1].getSimpleName().equalsIgnoreCase("string") && params[2].getSimpleName().equalsIgnoreCase("string")) {
                        a = (CharamelActObject) c.getDeclaredConstructor(params).newInstance(value1, value2, value3);
                    }
                }

            }
        } catch (ClassCastException | ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for Charamel Action " + cmd + " and value " + value1 + " and avatar id " + value3);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public CharamelActObject loadCharamelAnimation(String cmd, String... values) {
        CharamelActObject charamelActObject = null;

        String charamelCommandClasspath = getCharamelCommandClasspath(cmd);

        try {
            Class c = Class.forName(charamelCommandClasspath);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == values.length) {
                    boolean b = true;
                    for (Class param : params) {
                        if (!param.getSimpleName().equalsIgnoreCase("string")) {
                            b = false;
                            break;
                        }
                    }
                    if (b) {
                        charamelActObject = (CharamelActObject) c.getDeclaredConstructor(params).newInstance(values);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for Charamel Action " + cmd + " and value " + String.join(", ", values));
        }

        if (charamelActObject != null) {
            charamelActObject.setId(getNextID());
        }

        return charamelActObject;
    }

}
