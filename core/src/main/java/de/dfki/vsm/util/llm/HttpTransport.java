package de.dfki.vsm.util.llm;

import java.io.IOException;
import java.net.URI;
import java.time.Duration;
import java.util.Map;

/**
 * Small transport abstraction to keep LLM integration independent from a specific HTTP client.
 */
public interface HttpTransport {

    HttpResponseData get(URI uri, Map<String, String> headers, Duration timeout) throws IOException, InterruptedException;

    HttpResponseData postJson(URI uri, String body, Map<String, String> headers, Duration timeout)
            throws IOException, InterruptedException;

    record HttpResponseData(int statusCode, String body) {}
}
