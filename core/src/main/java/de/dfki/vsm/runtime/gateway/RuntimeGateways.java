package de.dfki.vsm.runtime.gateway;

import org.json.JSONObject;

import java.util.Locale;
import java.util.function.Function;

/**
 * Factory for platform-default runtime gateways.
 */
public final class RuntimeGateways {

    private RuntimeGateways() {
    }

    public static RuntimeGateway createDefault(final InProcessRuntimeGateway.CommandDispatcher commandDispatcher,
                                               final Function<String, JSONObject> snapshotProvider) {
        String mode = System.getProperty("vsm.runtime.gateway", "auto").trim().toLowerCase(Locale.ROOT);
        if ("disabled".equals(mode) || "none".equals(mode) || "off".equals(mode)) {
            return new DisabledRuntimeGateway();
        }
        if ("inprocess".equals(mode) || "local".equals(mode) || "on".equals(mode)) {
            return new InProcessRuntimeGateway(commandDispatcher, snapshotProvider);
        }
        if (isAndroidEnvironment()) {
            return new DisabledRuntimeGateway();
        }
        return new InProcessRuntimeGateway(commandDispatcher, snapshotProvider);
    }

    private static boolean isAndroidEnvironment() {
        String forcedPlatform = System.getProperty("vsm.platform", "").trim().toLowerCase(Locale.ROOT);
        if ("android".equals(forcedPlatform)) {
            return true;
        }
        String vmName = System.getProperty("java.vm.name", "").toLowerCase(Locale.ROOT);
        String runtimeName = System.getProperty("java.runtime.name", "").toLowerCase(Locale.ROOT);
        return vmName.contains("dalvik") || runtimeName.contains("android");
    }
}
