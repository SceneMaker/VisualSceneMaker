package de.dfki.vsm.xtension.voicerecognition;

import com.darkprograms.speech.microphone.Microphone;
import com.darkprograms.speech.recognizer.GSpeechDuplex;
import com.darkprograms.speech.recognizer.GSpeechResponseListener;
import com.darkprograms.speech.recognizer.GoogleResponse;
import de.dfki.vsm.runtime.project.RunTimeProject;
import net.sourceforge.javaflacencoder.FLACFileWriter;

import javax.sound.sampled.LineUnavailableException;
import java.io.IOException;

/**
 * Created by alvaro on 5/22/17.
 */
public class GoogleVoiceRecognition  implements VoiceRecognizer{

    private GSpeechDuplex duplex;
    private Microphone mic;
    private boolean isRecording;

    public GoogleVoiceRecognition(RunTimeProject mProject) {

    }

    public void init(){
        mic = new Microphone(FLACFileWriter.FLAC);
        duplex = new GSpeechDuplex("AIzaSyBOti4mM-6x9WDnZIjIeyEU21OpBXqWBgw");
        duplex.setLanguage("en");
    }


    private void registerListen() {
        duplex.addResponseListener(new CustomGoogleResponse());
    }

    public void startRecording() throws IOException, LineUnavailableException {
        init();
        registerListen();
        duplex.recognize(mic.getTargetDataLine(), mic.getAudioFormat());
    }

    @Override
    public void run() {
        try {
            startRecording();
            isRecording = true;
        } catch (IOException e) {
            e.printStackTrace();
        } catch (LineUnavailableException e) {
            e.printStackTrace();
        }
    }

    public void stopRecording(){
        mic.close();
        isRecording = false;
    }




    private class CustomGoogleResponse implements GSpeechResponseListener{
        @Override
        public void onResponse(GoogleResponse gr) {
            String output = "";
            output = gr.getResponse();
            if (gr.getResponse() == null) {

                System.out.println("Paragraph Line Added");
                //old_text = this.old_text.replace(")", "").replace("( ", "");
                return;
            }
            if (output.contains("(")) {
                output = output.substring(0, output.indexOf(40));
            }
            if (!gr.getOtherPossibleResponses().isEmpty()) {
                output = String.valueOf(output) + " (" + (String)gr.getOtherPossibleResponses().get(0) + ")";
            }else{
                int a = 0;
            }
            System.out.println(output);

        }

    }
}
