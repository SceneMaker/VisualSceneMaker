package de.dfki.vsm.util.http;

import org.junit.jupiter.api.Test;

import java.io.UnsupportedEncodingException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PostParameterTest {
    @Test
    void shouldAddParameter() throws UnsupportedEncodingException {
        PostParametersBuilder parameters = new PostParametersBuilder();
        parameters.addParameter("key", "value");
        String result = parameters.build();
        assertEquals("key=value&", result);
    }

    @Test
    void shouldAddParameterWithNoParameters() throws UnsupportedEncodingException {
        PostParametersBuilder parameters = new PostParametersBuilder();
        String result = parameters.build();
        assertEquals("", result);
    }

    @Test
    void shouldAddParameterWithMoreThanOneParameters() throws UnsupportedEncodingException {
        PostParametersBuilder parameters = new PostParametersBuilder();
        parameters.addParameter("key", "value");
        parameters.addParameter("key2", "value2");
        String result = parameters.build();
        assertEquals("key2=value2&key=value&", result);
    }
}