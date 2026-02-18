package de.dfki.vsm.util.llm;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Map;

/**
 * HttpURLConnection-based transport suitable for Android environments.
 */
public final class AndroidHttpTransport implements HttpTransport {

    @Override
    public HttpResponseData get(final URI uri, final Map<String, String> headers, final Duration timeout)
            throws IOException {
        HttpURLConnection conn = open(uri, "GET", headers, timeout);
        return execute(conn, null);
    }

    @Override
    public HttpResponseData postJson(final URI uri, final String body, final Map<String, String> headers,
                                     final Duration timeout) throws IOException {
        HttpURLConnection conn = open(uri, "POST", headers, timeout);
        return execute(conn, body != null ? body.getBytes(StandardCharsets.UTF_8) : new byte[0]);
    }

    private static HttpURLConnection open(final URI uri, final String method, final Map<String, String> headers,
                                          final Duration timeout) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) uri.toURL().openConnection();
        conn.setRequestMethod(method);
        int timeoutMs = Math.toIntExact(Math.max(1L, timeout.toMillis()));
        conn.setConnectTimeout(timeoutMs);
        conn.setReadTimeout(timeoutMs);
        conn.setUseCaches(false);
        conn.setDoInput(true);
        if ("POST".equals(method)) {
            conn.setDoOutput(true);
        }
        if (headers != null) {
            for (Map.Entry<String, String> entry : headers.entrySet()) {
                if (entry.getKey() != null && entry.getValue() != null) {
                    conn.setRequestProperty(entry.getKey(), entry.getValue());
                }
            }
        }
        return conn;
    }

    private static HttpResponseData execute(final HttpURLConnection conn, final byte[] body) throws IOException {
        try {
            if (body != null && body.length > 0) {
                try (OutputStream out = conn.getOutputStream()) {
                    out.write(body);
                }
            }
            int status = conn.getResponseCode();
            InputStream stream = status >= 400 ? conn.getErrorStream() : conn.getInputStream();
            String responseBody = readBody(stream);
            return new HttpResponseData(status, responseBody);
        } finally {
            conn.disconnect();
        }
    }

    private static String readBody(final InputStream stream) throws IOException {
        if (stream == null) {
            return "";
        }
        try (InputStream in = stream) {
            byte[] bytes = in.readAllBytes();
            return new String(bytes, StandardCharsets.UTF_8);
        }
    }
}
