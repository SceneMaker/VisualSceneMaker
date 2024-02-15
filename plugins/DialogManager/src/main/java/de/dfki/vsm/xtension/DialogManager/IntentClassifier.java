package de.dfki.vsm.xtension.DialogManager;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

public class IntentClassifier {
    DialogManagerExecutor executor;
    IntentClassifier() {
    }
    public void getIntent(String text) {
        try {
            // Define the URL
            URL url = new URL("https://www.dfki.de/ubidenz/model/parse");

            // Create HttpURLConnection object
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();

            // Set request method to POST
            connection.setRequestMethod("POST");

            // Set request headers
            connection.setRequestProperty("Content-Type", "application/json");

            // Enable output and input streams
            connection.setDoOutput(true);
            connection.setDoInput(true);

            // Create the JSON request body
            String requestBody = "{\"text\": \"" + text +"\"}";

            // Write the request body to the connection
            DataOutputStream outputStream = new DataOutputStream(connection.getOutputStream());
            outputStream.writeBytes(requestBody);
            outputStream.flush();
            outputStream.close();

            // Get the response code
            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);

            // Read the response from the server
            BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuilder response = new StringBuilder();

            while ((inputLine = in.readLine()) != null) {
                response.append(inputLine);
            }
            in.close();

            // Print the response
            System.out.println("Response: " + response.toString());

            // Close the connection
            connection.disconnect();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
