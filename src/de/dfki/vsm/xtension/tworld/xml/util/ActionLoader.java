/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.xtension.tworld.xml.util;

import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.xtension.tworld.xml.command.object.action.Action;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.LinkedList;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class ActionLoader {

    private final static String sTWORLDCMDPATH = "de.dfki.vsm.xtension.tworld.xml.command.object.action";
    private final static String sTWORLDCHARAMELCMDPATH = "de.dfki.vsm.xtension.tworld.xml.command.object.action.charamel";
    private static ActionLoader sInstance = null;
    private static long sID = 0;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    private ActionLoader() {
    }

    public static ActionLoader getInstance() {
        if (sInstance == null) {
            sInstance = new ActionLoader();
        }

        return sInstance;
    }

    public String getNextID() {
        sID++;
        return "tw" + sID;
    }

    private String getTWorldCommandClasspath(String cmd) {
        String classPath = "";

        classPath = sTWORLDCMDPATH + "." + cmd;

        try {
            Class.forName(classPath);
        } catch (ClassNotFoundException ex) {
            mLogger.failure("Wrong classpath for TWorld Action " + cmd);
        }

        return classPath;
    }

    private String getTWorldCharamelCommandClasspath(String cmd) {
        String classPath = "";

        classPath = sTWORLDCHARAMELCMDPATH + "." + cmd;

        try {
            Class.forName(classPath);
        } catch (ClassNotFoundException ex) {
            mLogger.failure("Wrong classpath for TWorld Action " + cmd);
        }

        return classPath;
    }

    public Action loadAnimation(String cmd) {
        Action a = null;

        String cp = getTWorldCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 0) {
                    a = (Action) c.getDeclaredConstructor(params).newInstance();

                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Action " + cmd);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public Action loadAnimation(String cmd, String value) {
        Action a = null;

        String cp = getTWorldCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 1) {
                    if (params[0].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Action " + cmd + " and value " + value);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public Action loadAnimation(String cmd, String value1, String value2) {
        Action a = null;

        String cp = getTWorldCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 2) {
                    if (params[0].getSimpleName().equalsIgnoreCase("string") && params[1].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value1, value2);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Action " + cmd + " and value " + value1);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public Action loadCharamelAnimation(String cmd, String value1) {
        Action a = null;

        String cp = getTWorldCharamelCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 1) {
                    if (params[0].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value1);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Charamel Action " + cmd + " and avatar id " + value1);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }
    
    public Action loadCharamelAnimation(String cmd, String value1, String value2) {
        Action a = null;

        String cp = getTWorldCharamelCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 2) {
                    if (params[0].getSimpleName().equalsIgnoreCase("string") && params[1].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value1, value2);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Charamel Action " + cmd + " and value " + value1 + " and avatar id " + value2);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public Action loadCharamelAnimation(String cmd, LinkedList value1, String value2, String value3) {
        Action a = null;

        String cp = getTWorldCharamelCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 3) {
                    if (params[0].getSimpleName().equalsIgnoreCase("linkedlist") && params[1].getSimpleName().equalsIgnoreCase("string") && params[2].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value1, value2, value3);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Charamel Action " + cmd + " and value " + value1 + " and avatar id " + value3);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }

    public Action loadAnimation(String cmd, String value1, String value2, String value3) {
        Action a = null;

        String cp = getTWorldCommandClasspath(cmd);

        try {
            Class c = Class.forName(cp);
            Constructor[] constructors = c.getConstructors();
            for (Constructor con : constructors) {
                Class[] params = con.getParameterTypes();
                if (params.length == 3) {
                    if (params[0].getSimpleName().equalsIgnoreCase("string") && params[1].getSimpleName().equalsIgnoreCase("string") && params[2].getSimpleName().equalsIgnoreCase("string")) {
                        a = (Action) c.getDeclaredConstructor(params).newInstance(value1, value2, value3);
                    }
                }

            }
        } catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            mLogger.failure("No Class for TWorld Action " + cmd + " and values " + value1 + ", " + value2 + ", " + value3);
        }

        if (a != null) {
            a.setId(getNextID());
        }

        return a;
    }
}
