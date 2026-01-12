package de.dfki.vsm.editor.connection;

import de.dfki.vsm.Preferences;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Manages multiple RuntimeConnection instances.
 * Provides methods for adding, removing, and selecting runtime connections.
 *
 * Connections are persisted to preferences and automatically restored on startup.
 *
 * @author Phase 5 Refactoring - 2026-01-12
 */
public class RuntimeConnectionManager {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();
    private static final String PREF_PREFIX = "runtime.connection.";
    private static final int MAX_CONNECTIONS = 10;

    private final Map<String, RuntimeConnection> mConnections = new ConcurrentHashMap<>();
    private String mActiveConnectionId;

    /**
     * Connection entry for persistence.
     */
    public static class ConnectionInfo {
        public final String id;
        public final String name;
        public final String url;
        public final String token;

        public ConnectionInfo(String id, String name, String url, String token) {
            this.id = id;
            this.name = name;
            this.url = url;
            this.token = token;
        }

        public JSONObject toJson() {
            JSONObject json = new JSONObject();
            json.put("id", id);
            json.put("name", name);
            json.put("url", url);
            json.put("token", token);
            return json;
        }

        public static ConnectionInfo fromJson(JSONObject json) {
            return new ConnectionInfo(
                json.optString("id", UUID.randomUUID().toString()),
                json.optString("name", ""),
                json.optString("url", ""),
                json.optString("token", "")
            );
        }
    }

    public RuntimeConnectionManager() {
        // Load saved connections from preferences
        loadConnections();
    }

    /**
     * Adds a new runtime connection.
     *
     * @param name Display name for the connection
     * @param url Runtime server URL (e.g., "http://192.168.1.100:8091")
     * @param token Authentication token
     * @return Connection ID
     */
    public String addConnection(String name, String url, String token) {
        String id = UUID.randomUUID().toString();

        RuntimeConnection connection = new RuntimeConnection(name, url, token);
        mConnections.put(id, connection);

        // Save to preferences
        saveConnections();

        sLogger.message("Added runtime connection: " + name + " at " + url);

        return id;
    }

    /**
     * Removes a runtime connection.
     *
     * @param connectionId Connection ID to remove
     * @return true if removed
     */
    public boolean removeConnection(String connectionId) {
        RuntimeConnection connection = mConnections.remove(connectionId);

        if (connection != null) {
            // Disconnect if connected
            if (connection.isConnected()) {
                connection.disconnect();
            }

            // Clear active connection if this was it
            if (connectionId.equals(mActiveConnectionId)) {
                mActiveConnectionId = null;
            }

            // Save to preferences
            saveConnections();

            sLogger.message("Removed runtime connection: " + connection.getName());
            return true;
        }

        return false;
    }

    /**
     * Gets a connection by ID.
     *
     * @param connectionId Connection ID
     * @return RuntimeConnection or null if not found
     */
    public RuntimeConnection getConnection(String connectionId) {
        return mConnections.get(connectionId);
    }

    /**
     * Gets all connections.
     *
     * @return Map of connection ID to RuntimeConnection
     */
    public Map<String, RuntimeConnection> getConnections() {
        return new HashMap<>(mConnections);
    }

    /**
     * Gets connection info for all connections.
     *
     * @return List of ConnectionInfo
     */
    public List<ConnectionInfo> getConnectionInfoList() {
        List<ConnectionInfo> list = new ArrayList<>();

        for (Map.Entry<String, RuntimeConnection> entry : mConnections.entrySet()) {
            RuntimeConnection conn = entry.getValue();
            list.add(new ConnectionInfo(
                entry.getKey(),
                conn.getName(),
                conn.getUrl(),
                "" // Don't expose token in list
            ));
        }

        return list;
    }

    /**
     * Sets the active runtime connection.
     *
     * @param connectionId Connection ID to set as active
     * @return true if successful
     */
    public boolean setActiveConnection(String connectionId) {
        if (connectionId == null) {
            mActiveConnectionId = null;
            return true;
        }

        if (mConnections.containsKey(connectionId)) {
            mActiveConnectionId = connectionId;
            sLogger.message("Set active runtime connection: " + mConnections.get(connectionId).getName());
            return true;
        }

        return false;
    }

    /**
     * Gets the active runtime connection.
     *
     * @return Active RuntimeConnection or null if none set
     */
    public RuntimeConnection getActiveConnection() {
        if (mActiveConnectionId == null) {
            return null;
        }
        return mConnections.get(mActiveConnectionId);
    }

    /**
     * Gets the active connection ID.
     *
     * @return Active connection ID or null if none set
     */
    public String getActiveConnectionId() {
        return mActiveConnectionId;
    }

    /**
     * Connects to a runtime server.
     *
     * @param connectionId Connection ID to connect
     * @return true if connection successful
     */
    public boolean connect(String connectionId) {
        RuntimeConnection connection = mConnections.get(connectionId);
        if (connection == null) {
            sLogger.failure("Connection not found: " + connectionId);
            return false;
        }

        return connection.connect();
    }

    /**
     * Disconnects from a runtime server.
     *
     * @param connectionId Connection ID to disconnect
     */
    public void disconnect(String connectionId) {
        RuntimeConnection connection = mConnections.get(connectionId);
        if (connection != null && connection.isConnected()) {
            connection.disconnect();
        }
    }

    /**
     * Disconnects all connections.
     */
    public void disconnectAll() {
        for (RuntimeConnection connection : mConnections.values()) {
            if (connection.isConnected()) {
                connection.disconnect();
            }
        }
    }

    /**
     * Gets connection status for all connections.
     *
     * @return JSON array of connection statuses
     */
    public JSONArray getConnectionStatus() {
        JSONArray statuses = new JSONArray();

        for (Map.Entry<String, RuntimeConnection> entry : mConnections.entrySet()) {
            JSONObject status = new JSONObject();
            status.put("id", entry.getKey());
            status.put("name", entry.getValue().getName());
            status.put("url", entry.getValue().getUrl());
            status.put("state", entry.getValue().getState().toString().toLowerCase());
            status.put("isActive", entry.getKey().equals(mActiveConnectionId));

            // Add runtime status if connected
            if (entry.getValue().isConnected()) {
                RuntimeConnection.RuntimeStatus runtimeStatus = entry.getValue().getStatus();
                if (runtimeStatus != null) {
                    JSONObject runtime = new JSONObject();
                    runtime.put("state", runtimeStatus.state);
                    runtime.put("projectPath", runtimeStatus.projectPath);
                    runtime.put("projectName", runtimeStatus.projectName);
                    runtime.put("isRunning", runtimeStatus.isRunning);
                    runtime.put("isPaused", runtimeStatus.isPaused);
                    status.put("runtime", runtime);
                }
            }

            statuses.put(status);
        }

        return statuses;
    }

    // ========== Persistence ==========

    /**
     * Loads connections from preferences.
     */
    private void loadConnections() {
        mConnections.clear();

        for (int i = 0; i < MAX_CONNECTIONS; i++) {
            String id = Preferences.getProperty(PREF_PREFIX + i + ".id");
            String name = Preferences.getProperty(PREF_PREFIX + i + ".name");
            String url = Preferences.getProperty(PREF_PREFIX + i + ".url");
            String token = Preferences.getProperty(PREF_PREFIX + i + ".token");

            if (id != null && !id.isEmpty() && url != null && !url.isEmpty()) {
                RuntimeConnection connection = new RuntimeConnection(
                    name != null ? name : "Runtime " + i,
                    url,
                    token != null ? token : ""
                );
                mConnections.put(id, connection);
            }
        }

        // Load active connection
        mActiveConnectionId = Preferences.getProperty(PREF_PREFIX + "active");

        if (!mConnections.isEmpty()) {
            sLogger.message("Loaded " + mConnections.size() + " runtime connection(s) from preferences");
        }
    }

    /**
     * Saves connections to preferences.
     */
    private void saveConnections() {
        // Clear existing entries
        for (int i = 0; i < MAX_CONNECTIONS; i++) {
            Preferences.removeProperty(PREF_PREFIX + i + ".id");
            Preferences.removeProperty(PREF_PREFIX + i + ".name");
            Preferences.removeProperty(PREF_PREFIX + i + ".url");
            Preferences.removeProperty(PREF_PREFIX + i + ".token");
        }

        // Save current connections
        int index = 0;
        for (Map.Entry<String, RuntimeConnection> entry : mConnections.entrySet()) {
            if (index >= MAX_CONNECTIONS) {
                break;
            }

            RuntimeConnection conn = entry.getValue();
            Preferences.setProperty(PREF_PREFIX + index + ".id", entry.getKey());
            Preferences.setProperty(PREF_PREFIX + index + ".name", conn.getName());
            Preferences.setProperty(PREF_PREFIX + index + ".url", conn.getUrl());
            // Note: We store token in preferences. In production, consider using secure storage.
            Preferences.setProperty(PREF_PREFIX + index + ".token", ""); // Token not persisted for security

            index++;
        }

        // Save active connection
        if (mActiveConnectionId != null) {
            Preferences.setProperty(PREF_PREFIX + "active", mActiveConnectionId);
        } else {
            Preferences.removeProperty(PREF_PREFIX + "active");
        }

        Preferences.save();
    }

    /**
     * Clears all connections (for testing).
     */
    public void clearAll() {
        disconnectAll();
        mConnections.clear();
        mActiveConnectionId = null;
        saveConnections();
    }
}
