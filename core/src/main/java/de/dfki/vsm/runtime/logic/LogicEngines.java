package de.dfki.vsm.runtime.logic;

import java.util.Objects;

/**
 * Global logic-engine registry with a desktop-default (JPL) engine.
 */
public final class LogicEngines {

    private static volatile LogicEngine sEngine = createDefault();

    private LogicEngines() {
    }

    public static LogicEngine get() {
        return sEngine;
    }

    public static void set(final LogicEngine engine) {
        sEngine = Objects.requireNonNull(engine, "engine");
    }

    private static LogicEngine createDefault() {
        final String value = System.getProperty("vsm.logic.engine", "jpl").trim().toLowerCase();
        if ("disabled".equals(value) || "none".equals(value) || "off".equals(value)) {
            return new DisabledLogicEngine();
        }
        final String vmName = System.getProperty("java.vm.name", "").toLowerCase();
        final String runtimeName = System.getProperty("java.runtime.name", "").toLowerCase();
        if (vmName.contains("dalvik") || runtimeName.contains("android")) {
            return new DisabledLogicEngine();
        }
        return createJplIfAvailable();
    }

    private static LogicEngine createJplIfAvailable() {
        try {
            Class<?> clazz = Class.forName("de.dfki.vsm.runtime.logic.JplLogicEngine");
            Object instance = clazz.getDeclaredConstructor().newInstance();
            if (instance instanceof LogicEngine) {
                return (LogicEngine) instance;
            }
        } catch (Throwable ignored) {
            // Fall back to disabled logic engine when the JPL adapter module is not on the classpath.
        }
        return new DisabledLogicEngine();
    }
}
