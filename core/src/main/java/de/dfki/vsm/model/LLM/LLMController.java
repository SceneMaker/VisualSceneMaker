package de.dfki.vsm.model.LLM;

import com.google.gson.Gson;
import de.dfki.vsm.model.LLM.JSON.ConversationIDResponse;
import de.dfki.vsm.model.LLM.JSON.LLMRequest;
import de.dfki.vsm.model.LLM.JSON.LLMResponse;
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

    private Gson gson;


    public LLMController() {
        gson = new Gson();
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
        try {
            URL url = new URL("http://172.16.58.75:5004/start_vsm");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            int responseCode = connection.getResponseCode();
            mLogger.message("LLM Response Code: " + responseCode);
            BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String line;
            StringBuilder responseBuilder = new StringBuilder();
            while ((line = reader.readLine()) != null) {
                responseBuilder.append(line);
            }
            String response = responseBuilder.toString();

            ConversationIDResponse idResponse = gson.fromJson(response, ConversationIDResponse.class);
            conversation_id = String.valueOf(idResponse.getConversationID());
            reader.close();
            connection.disconnect();
            mLogger.message("ConversationID: " + conversation_id);
            String primer = "Visual Scene Maker is an app that allows to generate a character interaction in different scenarios. One of the critical part of the app is the Scene which allows the users to define the interaction. I want to give you a scene with some instructions and i want you to generate multiple versions of that scene according to the instructions i give you. The scenes are written as in a theater script.\n" +
                    "The first line is the header just copy it from the given and put a that counts them behind the name(no blanks name1 name2 ...) The terms in [] are agent animations, use the ones from the given scene as you see fit but do not add others unless instructed to. " +
                    "Also only use the Agent names that are mentioned in the given scene.\n" +
                    "\n" +
                    "Only output the scene and NOT anything else.";
            getLLMResponse(primer);
            return;
        } catch (Exception e) {
            e.printStackTrace();
        }
        return;
    }

    public String getLLMResponse(String text) {
        try {
            URL url = new URL("http://172.16.58.75:5004/vsm_chat");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");
            String jsonInputString = gson.toJson(new LLMRequest(text,conversation_id));
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
            response = gson.fromJson(response, LLMResponse.class).getResponse();
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