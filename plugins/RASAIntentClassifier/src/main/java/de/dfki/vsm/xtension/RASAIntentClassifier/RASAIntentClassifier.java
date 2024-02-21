package de.dfki.vsm.xtension.RASAIntentClassifier;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import de.dfki.vsm.util.tpl.Tuple;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class RASAIntentClassifier {

    RASAIntentClassifier() {
    }

    public Tuple<String, String> getIntent(String text) {
        Tuple<String, String> intent = new Tuple<>("", "");

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
             intent = parseJson(response.toString());

            // Close the connection
            connection.disconnect();

        } catch (Exception e) {
            e.printStackTrace();
        }
        return intent;
    }

//    @Override
//    public final void start() {
//        super.start();
//    }

    public Tuple<String, String> parseJson(String jsonResponse) {
        String intent = "";
        String nameValue = "";
        try {
            // Parse JSON response
            JSONObject jsonObject = new JSONObject(jsonResponse);

            // Fetch intent
            JSONObject intentObject = jsonObject.getJSONObject("intent");
            intent = intentObject.getString("name");


            if (intent.equals("give_name")) {
                JSONArray entitiesArray = jsonObject.getJSONArray("entities");
                JSONObject nameEntity = entitiesArray.getJSONObject(0); // Assuming there's only one entity
                nameValue = nameEntity.getString("value");
            } else if (intent.equals("affirm")) {
                nameValue = "true";
            } else if (intent.equals("deny")) {
                nameValue = "false";
            }

            // Print intent
            System.out.println("Intent: " + intent);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        return new Tuple(intent, nameValue);
    }
}

