package de.dfki.vsm.xtension.IntentClassifier;

import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.json.JSONObject;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.stream.Collectors;


public class IntentClassifierJSONHandler extends AbstractHandler {
    private IntentClassifierExecutor executor;
    IntentClassifierJSONHandler(IntentClassifierExecutor executor) {
        this.executor = executor;
    }

    @Override
    public void handle(String target, Request baseRequest,
                       HttpServletRequest request, HttpServletResponse response)
            throws IOException, ServletException {

        // Set the response type to json
        response.setContentType("application/json;charset=utf-8");

        // Allow cross-origin requests
        response.setHeader("Access-Control-Allow-Origin", "*");
        response.setHeader("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE");
        response.setHeader("Access-Control-Max-Age", "3600");
        response.setHeader("Access-Control-Allow-Headers", "x-requested-with");

        // Only handle POST Requests
        // Only handle POST requests
        if (request.getMethod().equalsIgnoreCase("POST")) {
            // Read JSON request from the request body
            String requestBody = request.getReader().lines().collect(Collectors.joining());
            JSONObject requestJson = new JSONObject(requestBody);

            // Process the JSON request
            String classified_intent = requestJson.getString("intent");

            // Create a response JSON object
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "Agent received the event: " + classified_intent);

            // Handle everything inside the scene-maker!


            // Write the response JSON to the response body
            response.setStatus(HttpServletResponse.SC_OK);
            response.getWriter().println(responseJson.toString());
            baseRequest.setHandled(true);
        } else {
            // Return method not allowed for non-POST requests
            response.setStatus(HttpServletResponse.SC_METHOD_NOT_ALLOWED);
            baseRequest.setHandled(true);
        }
    }

    // Private functions to handle VSM stuffs
}
