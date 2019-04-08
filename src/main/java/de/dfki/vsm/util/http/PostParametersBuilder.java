package de.dfki.vsm.util.http;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static de.dfki.vsm.xtension.decad.utils.constants.Constants.UTF_8;

public class PostParametersBuilder {
    private static final String EQUAL_SEPARATOR = "=";
    private final HashMap<String, String> parameters = new HashMap<>();
    private StringBuilder parametersBuilder;

    public PostParametersBuilder addParameter(String key, String value) {
        parameters.put(key, value);
        return this;
    }

    public String build() throws UnsupportedEncodingException {
        buildParametersQuery();
        return parametersBuilder.toString();
    }

    public byte[] build(String charset) throws UnsupportedEncodingException {
        buildParametersQuery();
        return parametersBuilder.toString().getBytes(charset);
    }

    private void buildParametersQuery() throws UnsupportedEncodingException {
        parametersBuilder = new StringBuilder();
        for (Map.Entry<String, String> parameter : parameters.entrySet()) {
            addKeyValuePair(parameter);
            addAmpersandSeparatorIfNecessary();
        }
    }

    private void addKeyValuePair(Map.Entry<String, String> parameter) throws UnsupportedEncodingException {
        parametersBuilder.append(URLEncoder.encode(parameter.getKey(), StandardCharsets.UTF_8));
        parametersBuilder.append(EQUAL_SEPARATOR);
        parametersBuilder.append(URLEncoder.encode(String.valueOf(parameter.getValue()), StandardCharsets.UTF_8));
    }

    private void addAmpersandSeparatorIfNecessary() {
        if (parametersBuilder.length() != 0) parametersBuilder.append('&');
    }
}
