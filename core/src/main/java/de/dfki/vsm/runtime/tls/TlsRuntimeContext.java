package de.dfki.vsm.runtime.tls;

import java.util.List;

/**
 * Process-wide holder for the TLS material provisioned in {@code --secure} mode.
 *
 * <p>When the host is started with {@code --secure}, {@link MkcertProvisioner} creates
 * (or reuses) a locally-trusted mkcert certificate and records the resulting PKCS#12
 * keystore here. The web server ({@code core-webserver}) and the runtime plugins that
 * host their own HTTP servers (e.g. {@code htmlgui-ws}, {@code charamel-embed}) read
 * this holder to decide whether to serve over HTTPS/WSS, all sharing the one host
 * certificate so a remote browser trusts every origin after installing the CA once.
 *
 * <p>This class lives in {@code core} deliberately: the plugins depend on {@code core}
 * but not on {@code core-webserver}, so this is the only place all of them can reach.
 * It carries no Jetty/Javalin dependency — just the keystore coordinates each server
 * needs to build its own SSL connector.
 */
public final class TlsRuntimeContext {

    private static volatile boolean enabled = false;
    private static volatile String keyStorePath = null;
    private static volatile String keyStorePassword = null;
    private static volatile String rootCaPath = null;
    private static volatile List<String> subjectNames = List.of();

    private TlsRuntimeContext() {
    }

    /** True when {@code --secure} provisioning succeeded and servers should use HTTPS/WSS. */
    public static boolean isEnabled() {
        return enabled;
    }

    /** Absolute path to the shared PKCS#12 keystore holding the host certificate + key. */
    public static String getKeyStorePath() {
        return keyStorePath;
    }

    /** Password for the PKCS#12 keystore. */
    public static String getKeyStorePassword() {
        return keyStorePassword;
    }

    /** Absolute path to the mkcert root CA (rootCA.pem) collaborators install to trust the host. */
    public static String getRootCaPath() {
        return rootCaPath;
    }

    /** Host names / IPs the certificate is valid for (localhost, 127.0.0.1, LAN IP, hostname). */
    public static List<String> getSubjectNames() {
        return subjectNames;
    }

    /**
     * Records the provisioned TLS material and flips the process into secure mode.
     * Called once by {@link MkcertProvisioner} before any server starts.
     */
    public static synchronized void enable(String keyStorePath, String keyStorePassword,
                                           String rootCaPath, List<String> subjectNames) {
        TlsRuntimeContext.keyStorePath = keyStorePath;
        TlsRuntimeContext.keyStorePassword = keyStorePassword;
        TlsRuntimeContext.rootCaPath = rootCaPath;
        TlsRuntimeContext.subjectNames = (subjectNames != null) ? List.copyOf(subjectNames) : List.of();
        TlsRuntimeContext.enabled = true;
    }
}
