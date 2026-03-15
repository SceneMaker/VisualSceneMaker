package de.dfki.vsm.runtime.plugin;

/**
 * Optional interface for plugins that can self-report their health status.
 * Plugins implementing this will have their {@link #healthCheck()} called
 * by the Plugin Dashboard instead of the generic connectivity fallback.
 */
public interface PluginHealthCheckable {

    /**
     * Perform a health check and return the result.
     * Must return quickly (within ~2 seconds); avoid blocking operations.
     */
    HealthStatus healthCheck();

    record HealthStatus(boolean healthy, String message) {}
}
