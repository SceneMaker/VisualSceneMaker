package de.dfki.vsm.xtension.SIAHomeConnection;

import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.server.Request;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import org.json.JSONObject;
import org.json.JSONObject;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


public class SIAHomeConnectionJSONHandler extends AbstractHandler {
    SIAHomeConnectionJSONHandler(SIAHomeConnectionExecutor executor) {
        this.executor = executor;
    }
    private SIAHomeConnectionExecutor executor;

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
        if (request.getMethod().equalsIgnoreCase("POST")) {
            // Read JSON request from the request body
            String requestBody = request.getReader().lines().collect(Collectors.joining());
            JSONObject requestJson = new JSONObject(requestBody);

            // Process the JSON request
            String bhome_event = requestJson.getString("event");
            String createdToday = requestJson.getString("createdAt");
            String sleepYesterday = requestJson.getString("sleepAt");
            String wakeUpToday = requestJson.getString("wakeUpAt");
            boolean nightActivity = requestJson.getBoolean("activityAt");
            String userName = requestJson.getString("userName");

            System.out.println("[SIAHome]: bhome_event: " + bhome_event);
            System.out.println("[SIAHome]: createdToday: " + createdToday);
            System.out.println("[SIAHome]: sleepYesterday: " + sleepYesterday);
            System.out.println("[SIAHome]: wakeUpToday: " + wakeUpToday);
            System.out.println("[SIAHome]: nightActivity: " + nightActivity);
            System.out.println("[SIAHome]: user_name: " + userName);

            // Create a response JSON object
            JSONObject responseJson = new JSONObject();
            responseJson.put("message", "Agent received the event: " + bhome_event);
            responseJson.put("message", "Agent received the createdToday: " + createdToday);
            responseJson.put("message", "Agent received the sleepYesterday: " + sleepYesterday);
            responseJson.put("message", "Agent received the wakeUpToday: " + wakeUpToday);
            responseJson.put("message", "Agent received the event: " +  nightActivity);
            responseJson.put("message", "Agent received the user_name: " + userName);


            // Handle everything inside the scene-maker!
            // Create a ScheduledExecutorService
            ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();

            ScheduledFuture<?>[] futureHolder = new ScheduledFuture<?>[1];
            AtomicInteger period = new AtomicInteger(0);

            // Schedule a task to check the timeout_response flag every second for 120 seconds
            futureHolder[0] = executorService.scheduleAtFixedRate(() -> {
                // Check if the timeout_response flag is true
                Boolean timeout_response = ((Boolean) this.executor.getVariable());
                period.incrementAndGet();
                System.out.println("Timeout is: " + timeout_response + " " + period);
                if (!timeout_response || period.get() == 120) {
                    // If it's been 120 seconds and the flag is still true, set the response to a timeout response
                    responseJson.put("message", "Timeout");
                    // Cancel the task
                    futureHolder[0].cancel(false);
                    executorService.shutdown();
                }
            }, 0, 1, TimeUnit.SECONDS);

            try {
                executorService.awaitTermination(120, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }

            // Define the input and output date-time format
            DateTimeFormatter inputFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
            DateTimeFormatter outputFormatter = DateTimeFormatter.ofPattern("HH:mm");

            // Parse the datetime strings and format them to extract only the time
            String createdTime = LocalDateTime.parse(createdToday, inputFormatter).format(outputFormatter);
            String sleepYesterdayTime = LocalDateTime.parse(sleepYesterday, inputFormatter).format(outputFormatter);
            String wakeUpTime = LocalDateTime.parse(wakeUpToday, inputFormatter).format(outputFormatter);

            // Print the extracted times
            System.out.println("Wake up today time: " + createdTime);
            System.out.println("Sleep yesterday time: " + sleepYesterdayTime);
            System.out.println("Wake up yesterday time: " + wakeUpTime);


            executor.setVariable("bhome_event", bhome_event);
            executor.setVariable("createdAt", createdTime);
            executor.setVariable("sleepAt", sleepYesterdayTime);
            executor.setVariable("wakeupAt", wakeUpTime);
            executor.setVariable("activityAt", Boolean.toString(nightActivity));
            executor.setVariable("user_name", userName);

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

}
