package de.dfki.vsm.model.LLM;

import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

public class LLMController {
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private String conversation_id;

    public LLMController() {
    }

    //this function is called when the play button in pressed in the LLM registry panel
    //the input text is from is the content of the textfield
    public void execute(String text){

        mLogger.warning("LLMController was called with: " + text );
        startSession();
        String response = getLLMResponse(text);
        new LLMResponseWindow(response);
    }




    public void startSession(){
        conversation_id="";
    }
    public String getLLMResponse(String text) {
        try {
            URL url = new URL("https://example.com/api/login");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            String jsonInputString = "{\"text\": \""+text+"\", \"conversation_id\": \""+conversation_id+"\"}";

            try (DataOutputStream wr = new DataOutputStream(connection.getOutputStream())) {
                byte[] input = jsonInputString.getBytes(StandardCharsets.UTF_8);
                wr.write(input, 0, input.length);
            }

            int responseCode = connection.getResponseCode();
            mLogger.message("LLM Response Code: " + responseCode);
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String line;
            StringBuilder responseBuilder = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                responseBuilder.append(line);
            }
            String response = responseBuilder.toString();

            reader.close();
            connection.disconnect();
            mLogger.message("LLM Response: " + response);
            return response;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return "";
    }
}
