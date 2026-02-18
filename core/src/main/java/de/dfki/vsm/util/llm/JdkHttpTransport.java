package de.dfki.vsm.util.llm;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Map;
import java.util.Objects;

/**
 * JDK HttpClient based transport implementation for desktop/server runtime.
 */
public final class JdkHttpTransport implements HttpTransport {

    private final HttpClient mHttpClient;

    public JdkHttpTransport() {
        this(HttpClient.newBuilder()
                .version(HttpClient.Version.HTTP_1_1)
                .connectTimeout(Duration.ofSeconds(10))
                .build());
    }

    public JdkHttpTransport(final HttpClient httpClient) {
        mHttpClient = Objects.requireNonNull(httpClient, "httpClient");
    }

    @Override
    public HttpResponseData get(final URI uri, final Map<String, String> headers, final Duration timeout)
            throws IOException, InterruptedException {
        final HttpRequest.Builder builder = HttpRequest.newBuilder(uri)
                .timeout(timeout)
                .version(HttpClient.Version.HTTP_1_1)
                .GET();
        applyHeaders(builder, headers);
        final HttpResponse<String> response = mHttpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString());
        return new HttpResponseData(response.statusCode(), response.body());
    }

    @Override
    public HttpResponseData postJson(final URI uri, final String body, final Map<String, String> headers,
                                     final Duration timeout) throws IOException, InterruptedException {
        final HttpRequest.Builder builder = HttpRequest.newBuilder(uri)
                .timeout(timeout)
                .version(HttpClient.Version.HTTP_1_1)
                .POST(HttpRequest.BodyPublishers.ofString(body));
        applyHeaders(builder, headers);
        final HttpResponse<String> response = mHttpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString());
        return new HttpResponseData(response.statusCode(), response.body());
    }

    private static void applyHeaders(final HttpRequest.Builder builder, final Map<String, String> headers) {
        if (headers == null || headers.isEmpty()) {
            return;
        }
        for (Map.Entry<String, String> entry : headers.entrySet()) {
            if (entry.getKey() != null && entry.getValue() != null) {
                builder.header(entry.getKey(), entry.getValue());
            }
        }
    }
}
