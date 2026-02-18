package de.dfki.vsm.runtime.bootstrap;

import de.dfki.vsm.runtime.logic.DisabledLogicEngine;
import de.dfki.vsm.runtime.logic.LogicEngines;
import de.dfki.vsm.util.llm.HttpTransport;
import de.dfki.vsm.util.llm.LLMSupport;

/**
 * Platform-specific bootstrap hooks for runtime composition defaults.
 */
public final class PlatformBootstrap {

    private static volatile boolean sConfigured;

    private PlatformBootstrap() {
    }

    public static synchronized void configureForCurrentVm() {
        if (sConfigured) {
            return;
        }
        if (isAndroidEnvironment()) {
            configureForAndroid();
        }
        sConfigured = true;
    }

    public static synchronized void configureForAndroid() {
        LogicEngines.set(new DisabledLogicEngine());
        LLMSupport.setDefaultTransportFactory(() -> instantiateTransport("de.dfki.vsm.util.llm.AndroidHttpTransport"));
    }

    private static HttpTransport instantiateTransport(final String className) {
        try {
            Class<?> clazz = Class.forName(className);
            Object instance = clazz.getDeclaredConstructor().newInstance();
            if (instance instanceof HttpTransport) {
                return (HttpTransport) instance;
            }
            throw new IllegalStateException(className + " is not an HttpTransport");
        } catch (Exception exc) {
            throw new IllegalStateException("Failed to initialize transport: " + className, exc);
        }
    }

    private static boolean isAndroidEnvironment() {
        String forced = System.getProperty("vsm.platform", "").trim().toLowerCase();
        if ("android".equals(forced)) {
            return true;
        }
        String vmName = System.getProperty("java.vm.name", "").toLowerCase();
        String runtimeName = System.getProperty("java.runtime.name", "").toLowerCase();
        return vmName.contains("dalvik") || runtimeName.contains("android");
    }
}
