package de.dfki.vsm.runtime.tls;

import de.dfki.vsm.util.log.LOGDefaultLogger;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Provisions a locally-trusted host certificate with <a href="https://github.com/FiloSottile/mkcert">mkcert</a>
 * for {@code --secure} mode, then records it in {@link TlsRuntimeContext}.
 *
 * <p>Steps performed on {@link #provision()}:
 * <ol>
 *   <li>locate the {@code mkcert} binary (PATH + common install locations);</li>
 *   <li>{@code mkcert -install} — create the local CA (if absent) and trust it on this host;</li>
 *   <li>{@code mkcert -pkcs12} — issue a host cert covering localhost, 127.0.0.1 and the LAN
 *       address, packaged as a PKCS#12 keystore the Jetty servers load directly;</li>
 *   <li>resolve {@code rootCA.pem} (via {@code mkcert -CAROOT}) for later distribution to
 *       collaborators, and flip {@link TlsRuntimeContext} into secure mode.</li>
 * </ol>
 *
 * <p>If mkcert is not installed the provisioner fails softly: it logs a one-line install hint
 * and returns {@code false} so the caller can fall back to plain HTTP.
 */
public final class MkcertProvisioner {

    private static final LOGDefaultLogger sLogger = LOGDefaultLogger.getInstance();

    /** mkcert's default password for {@code -pkcs12} output. */
    private static final String P12_PASSWORD = "changeit";

    private MkcertProvisioner() {
    }

    /**
     * Ensures a host certificate exists and enables {@link TlsRuntimeContext}.
     *
     * @return {@code true} if secure mode is now active, {@code false} if it could not be set up.
     */
    public static boolean provision() {
        String mkcert = locateMkcert();
        if (mkcert == null) {
            sLogger.warning("--secure: mkcert not found. Install it once, e.g. "
                    + "'brew install mkcert nss' (macOS), 'choco install mkcert' (Windows), "
                    + "or see https://github.com/FiloSottile/mkcert. Falling back to HTTP.");
            return false;
        }

        // 1. Create + trust the local CA (idempotent).
        // The very first run may prompt for a password to install the CA into the system
        // trust store — which cannot be answered when launched from Gradle. If that
        // happens this call times out; run 'mkcert -install' once manually in a terminal.
        ExecResult install = run(mkcert, "-install");
        if (install.exitCode != 0) {
            sLogger.failure("--secure: 'mkcert -install' failed (" + install.output.trim() + ")."
                    + " Run 'mkcert -install' once manually in a terminal (it may ask for your"
                    + " password the first time), then start again. Falling back to HTTP.");
            return false;
        }

        // 2. Resolve the CA root so we can distribute rootCA.pem to collaborators.
        ExecResult caroot = run(mkcert, "-CAROOT");
        String rootCaPath = null;
        if (caroot.exitCode == 0) {
            File pem = new File(caroot.output.trim(), "rootCA.pem");
            if (pem.isFile()) {
                rootCaPath = pem.getAbsolutePath();
            }
        }

        // 3. Issue the host certificate as a PKCS#12 keystore.
        List<String> names = collectSubjectNames();
        File tlsDir = new File(System.getProperty("user.home"), ".vsm.d" + File.separator + "tls");
        if (!tlsDir.isDirectory() && !tlsDir.mkdirs()) {
            sLogger.warning("--secure: cannot create " + tlsDir + " — falling back to HTTP.");
            return false;
        }
        File p12 = new File(tlsDir, "vsm-host.p12");

        List<String> args = new ArrayList<>();
        args.add("-pkcs12");
        args.add("-p12-file");
        args.add(p12.getAbsolutePath());
        args.addAll(names);
        ExecResult gen = run(mkcert, args.toArray(new String[0]));
        if (gen.exitCode != 0 || !p12.isFile()) {
            sLogger.warning("--secure: certificate generation failed: " + gen.output.trim()
                    + " — falling back to HTTP.");
            return false;
        }

        TlsRuntimeContext.enable(p12.getAbsolutePath(), P12_PASSWORD, rootCaPath, names);
        sLogger.message("--secure: HTTPS enabled for " + String.join(", ", names)
                + (rootCaPath != null ? " (CA: " + rootCaPath + ")" : ""));
        return true;
    }

    /** Names + IPs the host certificate should cover. */
    private static List<String> collectSubjectNames() {
        Set<String> names = new LinkedHashSet<>();
        try {
            String host = InetAddress.getLocalHost().getHostName();
            if (host != null && !host.isBlank()) {
                names.add(host);
                if (!host.endsWith(".local")) {
                    names.add(host + ".local");
                }
            }
        } catch (Exception ignored) {
            // Hostname is best-effort; the LAN IP below is what remote clients actually use.
        }
        names.add("localhost");
        names.add("127.0.0.1");
        names.add("::1");
        String lan = findLanAddress();
        if (lan != null) {
            names.add(lan);
        }
        return new ArrayList<>(names);
    }

    /** First non-loopback site-local IPv4 address (the LAN IP a remote browser connects to). */
    private static String findLanAddress() {
        try {
            for (NetworkInterface nif : java.util.Collections.list(NetworkInterface.getNetworkInterfaces())) {
                if (!nif.isUp() || nif.isLoopback() || nif.isVirtual()) {
                    continue;
                }
                for (InetAddress addr : java.util.Collections.list(nif.getInetAddresses())) {
                    if (addr.isSiteLocalAddress() && addr.getAddress().length == 4) {
                        return addr.getHostAddress();
                    }
                }
            }
        } catch (Exception ignored) {
            // No LAN address is fine — localhost still works on the host itself.
        }
        return null;
    }

    /**
     * Finds the mkcert executable on PATH or in common install locations by running
     * {@code <candidate> -CAROOT} and accepting the first that exits cleanly.
     */
    private static String locateMkcert() {
        List<String> candidates = new ArrayList<>();
        candidates.add("mkcert");
        candidates.add("/opt/homebrew/bin/mkcert");
        candidates.add("/usr/local/bin/mkcert");
        candidates.add("/usr/bin/mkcert");
        candidates.add("mkcert.exe");
        for (String candidate : candidates) {
            try {
                ExecResult r = run(candidate, "-CAROOT");
                if (r.exitCode == 0) {
                    return candidate;
                }
            } catch (Exception ignored) {
                // try next candidate
            }
        }
        return null;
    }

    private static ExecResult run(String command, String... args) {
        List<String> cmd = new ArrayList<>();
        cmd.add(command);
        for (String a : args) {
            cmd.add(a);
        }
        try {
            ProcessBuilder pb = new ProcessBuilder(cmd);
            pb.redirectErrorStream(true);
            Process p = pb.start();
            StringBuilder out = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(p.getInputStream(), StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    out.append(line).append('\n');
                }
            }
            boolean done = p.waitFor(60, TimeUnit.SECONDS);
            if (!done) {
                p.destroyForcibly();
                return new ExecResult(-1, "timed out");
            }
            return new ExecResult(p.exitValue(), out.toString());
        } catch (Exception e) {
            return new ExecResult(-1, e.getMessage() == null ? e.toString() : e.getMessage());
        }
    }

    private static final class ExecResult {
        final int exitCode;
        final String output;

        ExecResult(int exitCode, String output) {
            this.exitCode = exitCode;
            this.output = output == null ? "" : output;
        }
    }
}
