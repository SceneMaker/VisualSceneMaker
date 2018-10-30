/**
 * Copyright 2000-2007 DFKI GmbH.
 * All Rights Reserved.  Use is subject to license terms.
 * 
 * Permission is hereby granted, free of charge, to use and distribute
 * this software and its documentation without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of this work, and to
 * permit persons to whom this work is furnished to do so, subject to
 * the following conditions:
 * 
 * 1. The code must retain the above copyright notice, this list of
 *    conditions and the following disclaimer.
 * 2. Any modifications must be clearly marked as such.
 * 3. Original authors' names are not deleted.
 * 4. The authors' names are not used to endorse or promote products
 *    derived from this software without specific prior written
 *    permission.
 *
 * DFKI GMBH AND THE CONTRIBUTORS TO THIS WORK DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL DFKI GMBH NOR THE
 * CONTRIBUTORS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */
package de.dfki.vsm.util.tts.marytts;


import de.dfki.common.Gender;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.tts.SpeechClient;
import de.dfki.vsm.util.tts.VoiceName;
import de.dfki.vsm.util.tts.marytts.phonemes.EnglishPhoneme;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.AudioClosed;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.AudioOpened;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStart;
import de.dfki.vsm.xtension.stickmantts.util.tts.events.LineStop;
import de.dfki.vsm.xtension.stickmantts.util.tts.sequence.Phoneme;
import marytts.client.MaryClient;
import marytts.util.data.audio.AudioPlayer;
import marytts.util.http.Address;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.sound.sampled.*;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;


public class I4GMaryClient extends SpeechClient {

    private MaryClient maryClient = null;

    public static final VoiceName FEMALE = new VoiceName("dfki-poppy");
    public static final VoiceName MALE = new VoiceName("dfki-obadiah");
    public static final VoiceName DE1 = new VoiceName("dfki-poker");
    public static final VoiceName DE2 = new VoiceName("hmm-bits2");
    public static final VoiceName DE3 = new VoiceName("hmm-bits3");
    public static final VoiceName DE4 = new VoiceName("de4");
    public static final VoiceName DE5 = new VoiceName("de5");
    public static final VoiceName DE6 = new VoiceName("de6");
    public static final VoiceName DE7 = new VoiceName("de7");
    public static final VoiceName US1 = new VoiceName("us1");
    public static final VoiceName US2 = new VoiceName("us2");
    public static final VoiceName US3 = new VoiceName("us3");
    public static final VoiceName BITS1 = new VoiceName("bits1");
    public static final VoiceName BITS2 = new VoiceName("bits2");
    public static final VoiceName BITS3 = new VoiceName("bits3");
    public static final VoiceName BITS4 = new VoiceName("bits4");
    public static final VoiceName POPPY = new VoiceName("dfki-poppy");
    public static final VoiceName SPIKE = new VoiceName("dfki-spike");
    public static final VoiceName OBADIAH = new VoiceName("dfki-obadiah");
    public static final VoiceName CMU = new VoiceName("cmu-slt-hsmm");
    private static final String SAMPLE_TEXT = "Willkommen in der Welt der Sprachsynthese!";
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();
    // Singleton
    static private I4GMaryClient instance = null;
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private String speak_text = "";
    private HashMap<String, Language> languageMap = new HashMap<>();


    private I4GMaryClient() throws IOException{
        
        this(System.getProperty("server.host", "localhost"), Integer.getInteger("server.port", 59125).intValue());
       // wordQueue = new LinkedList<>();
        wordQueue =  Collections.synchronizedList(new LinkedList());
        finalWord = "";
        
    }

    private I4GMaryClient(String host, int port) throws IOException{
        Address address = new Address(host, port);
        this.maryClient = maryClient.getMaryClient(address);
        languageMap.put("en", new Language("en-GB"));
        languageMap.put("de", new Language("de"));
    }
    
    

    public static synchronized void create() {
        if (instance == null) {
            try {
                instance = new I4GMaryClient();
                
            } catch (IOException ex) {
                Logger.getLogger(I4GMaryClient.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

    public static I4GMaryClient instance() {
        if (instance == null) {
            create();
        }
        return instance;
    }

    public void playAudio(ByteArrayOutputStream baos) throws UnsupportedAudioFileException, IOException {
        AudioInputStream ais = AudioSystem.getAudioInputStream(
                new ByteArrayInputStream(baos.toByteArray()));
        playAudio(ais);
    }

    public void playAudio(ByteArrayOutputStream baos, String executionId) throws UnsupportedAudioFileException, IOException {
        AudioInputStream ais = AudioSystem.getAudioInputStream(
                new ByteArrayInputStream(baos.toByteArray()));
        playAudio(ais, executionId);
    }

    public void playAudio(AudioInputStream ais, String executionId) {
        LineListener lineListener = new LineListener() {

            public void update(LineEvent event) {
                if (event.getType() == LineEvent.Type.START) {
                    //mEventCaster.convey(new LineStart(this,  executionId));
                } else if (event.getType() == LineEvent.Type.STOP) {
                    mEventCaster.convey(new LineStop(this, executionId));
                    mLogger.message("Audio stopped playing.");
                } else if (event.getType() == LineEvent.Type.OPEN) {
                    mLogger.message("Audio opened Line.");
                    mEventCaster.convey(new LineStart(this,  executionId));
                    //mEventCaster.convey(new AudioOpened(this));
                } else if (event.getType() == LineEvent.Type.CLOSE) {
                    mEventCaster.convey(new AudioClosed(this));
                }
            }
        };
        AudioPlayer ap = new AudioPlayer(ais, lineListener);
        ap.start();
    }

    public void playAudio(AudioInputStream ais) {
        LineListener lineListener = new LineListener() {

            public void update(LineEvent event) {
                if (event.getType() == LineEvent.Type.START) {
                    mEventCaster.convey(new LineStart(this));
                } else if (event.getType() == LineEvent.Type.STOP) {
                    mEventCaster.convey(new LineStop(this));
                    mLogger.message("Audio stopped playing.");
                } else if (event.getType() == LineEvent.Type.OPEN) {
                    mEventCaster.convey(new AudioOpened(this));
                    mLogger.message("Audio line opened.");
                } else if (event.getType() == LineEvent.Type.CLOSE) {
                    mEventCaster.convey(new AudioClosed(this));
                    mLogger.message("Audio line closed.");
                }
            }
        };
        AudioPlayer ap = new AudioPlayer(ais, lineListener);
        ap.start();
    }

    public String getRawMaryXml(String speaker, String text, Language lang) {
        StringBuffer buffer = new StringBuffer();
        buffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n");
        buffer.append("<maryxml version=\"0.5\"\n");
        buffer.append("\txmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n");
        buffer.append("\txmlns=\"http://mary.dfki.de/2002/MaryXML\"\n");
        buffer.append("\txml:lang=\"");
        buffer.append(lang.toString());
        buffer.append("\">\n");
        buffer.append(text);
        buffer.append("\n</maryxml>");
        return buffer.toString();

    }

    public String getAcoustParams(String speaker, String rawMaryXml, VoiceName voice, String language) throws IOException, UnknownHostException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        if(language == null || language.equals("")){
            language = "en";
        }
        // emotional processing ... set new voices and effects
        String voiceName = voice.toString();
        
        String effects = "";

        // process (old version)
        //maryClient.process(rawMaryXml, DataType.RAWMARYXML.toString(), DataType.ACOUSTPARAMS.toString(), null, voice.toString(), baos); // ggf. AudioType.WAVE
        maryClient.process(rawMaryXml, "RAWMARYXML", "ACOUSTPARAMS", languageMap.get(language).toString(),  "WAVE", voiceName,  baos); // ggf. AudioType.WAVE
         
        String acoustParams = baos.toString();
        baos.close();
        return acoustParams;
    }

    public String getPhoneticParams(String speaker, String rawMaryXml, VoiceName voice, String language) throws IOException, UnknownHostException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // emotional processing ... set new voices and effects
        String voiceName = voice.toString();

        if(language == null || language.equals("")){
            language = "en";
        }
        String effects = "";
        maryClient.process(rawMaryXml, "RAWMARYXML", "ALLOPHONES", languageMap.get(language).toString(),  "WAVE", voiceName,  baos); // ggf. AudioType.WAVE

        String acoustParams = baos.toString();
        baos.close();
        return acoustParams;
    }
    
    public ByteArrayOutputStream getAudio(String speaker, String rawMaryXml, VoiceName voice, String language) throws IOException, UnknownHostException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // emotional processing ... set new voices and effects
        if(language == null || language.equals("")){
            language = "en";
        }
        String voiceName = voice.toString();
        String effects = "";

        
        //maryClient.process(rawMaryXml, DataType.RAWMARYXML.toString(), DataType.AUDIO.toString(), AudioType.WAVE.toString(), voice.toString(), baos);
          maryClient.process(rawMaryXml, "ACOUSTPARAMS", "AUDIO", languageMap.get(language).toString(),  "WAVE", voiceName,  baos); // ggf. AudioType.WAVE
        
//System.out.println("length of baos " + baos.size());
          baos.close();
        return baos;
    }


    
    public void addWord(String word){
        synchronized(wordQueue){
            wordQueue.add(word);
            this.speak_text = "";
        }
    }
    
    public String getText(){
        if(this.speak_text.equals("")){
            this.speak_text = this.getPhrase();
        }
        return this.speak_text;
    }
    

    
    public long getPhraseTime(VoiceName voiceName, String language) throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception { //TODO: Get language from scene
        if(language == null || language.equals("")){
            language = "en";
        }
        String text = this.getPhrase();
        String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
        String acoustParams = getAcoustParams("", rawMaryXml, voiceName,language);
        InputStream stream = new ByteArrayInputStream(acoustParams.getBytes(StandardCharsets.UTF_8));
        String endTime = parseAcoustParams(stream);
        if(!endTime.equals("")){
            float value = Float.parseFloat(endTime);
            value*= 1000;
            return  (long)(Math.ceil(value));
        }
        return (long)0;
    }
    
    private String parseAcoustParams(InputStream paramsXML) throws ParserConfigurationException, SAXException, IOException, XPathExpressionException{
        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(paramsXML);

        doc.getDocumentElement().normalize();
        Node root = doc.getDocumentElement();
        XPathFactory xFactory = XPathFactory.newInstance();
        XPath xPath = xFactory.newXPath();
        XPathExpression xExpress = xPath.compile("//ph[position() = last()]");
        System.out.println("Root element :" + doc.getDocumentElement().getNodeName());
        NodeList nodes = (NodeList) xExpress.evaluate(root, XPathConstants.NODESET);
        Node lastNode = nodes.item(nodes.getLength()-1);
        String endTime = lastNode.getAttributes().item(1).getNodeValue();
        return endTime;

    }

    private LinkedList parseAcoustPhonems(InputStream paramsXML) throws ParserConfigurationException, SAXException, IOException, XPathExpressionException{
        LinkedList<Phoneme> phonemes = new LinkedList<>();

        DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(paramsXML);

        doc.getDocumentElement().normalize();
        Node root = doc.getDocumentElement();
        XPathFactory xFactory = XPathFactory.newInstance();
        XPath xPath = xFactory.newXPath();
        XPathExpression xExpress = xPath.compile("//ph");
        System.out.println("Root element :" + doc.getDocumentElement().getNodeName());
        NodeList nodes = (NodeList) xExpress.evaluate(root, XPathConstants.NODESET);
        long currentTime = 0;
        long startTime = 0 ;
        for(int i =0; i< nodes.getLength(); i++){
            long endT = 0;
            String endTime = nodes.item(i).getAttributes().getNamedItem("end").getNodeValue();;
            if(!endTime.equals("")){
                float end = Float.parseFloat(endTime);
                end*= 1000;
                endT = (long)(Math.ceil(end));
            }
            final String value = nodes.item(i).getAttributes().getNamedItem("p").getNodeValue();
            currentTime+= endT;
            phonemes.add(new EnglishPhoneme(value, startTime, endT));
            startTime = endT;
        }
        return phonemes;

    }
    
    public void clearWordList(){
        synchronized(wordQueue) {
            wordQueue.clear();
            speak_text = "";
            finalWord = "";
        }
        
    }
    
    public void speak(String text, VoiceName voice, String language)
            throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {

        if(language == null || language.equals("")){
            language = "en";
        }
        String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
        String acoustParams = getAcoustParams("", rawMaryXml, voice,language);
        ByteArrayOutputStream audio = getAudio("", acoustParams, voice,language);
        playAudio(audio);
        //this.clearWordList();
    }

    public String getPhonetics(String word, String language) throws IOException { //Get phonetic information from a word
        if(language == null || language.equals("")){
            language = "en";
        }
        String rawMaryXml = getRawMaryXml("", word, languageMap.get(language));
        String phoneticParams = getPhoneticParams("", rawMaryXml, OBADIAH,language);
        return phoneticParams;
    }

    public String getAllCousticParms(String word, Gender.TYPE gender, VoiceName voiceName, String language) throws IOException { //Get phonetic information from a word
        if(language == null || language.equals("")){
            language = "en";
        }
        VoiceName speakerVoice = null;
        speakerVoice = getSpeakingVoice(gender, voiceName);
        String rawMaryXml = getRawMaryXml("", word, languageMap.get(language));
        String phoneticParams = getAcoustParams("", rawMaryXml, speakerVoice,language);
        return phoneticParams;
    }

    public long getWordDuration(String word, Gender.TYPE gender, VoiceName voiceName, String language) throws IOException {
        String acousticParams = getAllCousticParms(word, gender, voiceName, language);
        InputStream stream = new ByteArrayInputStream(acousticParams.getBytes(StandardCharsets.UTF_8));
        String endTime = null;
        try {
            endTime = parseAcoustParams(stream);
        } catch (ParserConfigurationException e) {
            e.printStackTrace();
        } catch (SAXException e) {
            e.printStackTrace();
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        if(!endTime.equals("")){
            float value = Float.parseFloat(endTime);
            value*= 1000;
            return  (long)(Math.ceil(value));
        }
        return (long)0;
    }

    public LinkedList getWordPhonemeList(String word, Gender.TYPE gender, VoiceName voiceName, String language) throws IOException {
        String acousticParams = getAllCousticParms(word, gender, voiceName, language);
        InputStream stream = new ByteArrayInputStream(acousticParams.getBytes(StandardCharsets.UTF_8));
        String endTime = null;
        LinkedList<Phoneme> phonemes = new LinkedList<>();
        try {
            phonemes = parseAcoustPhonems(stream);
        } catch (ParserConfigurationException e) {
            e.printStackTrace();
        } catch (SAXException e) {
            e.printStackTrace();
        } catch (XPathExpressionException e) {
            e.printStackTrace();
        }
        return phonemes;
    }
    
    public void speak(String language) throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        String text = this.getPhrase();
        this.clearWordList();
        if(language == null || language.equals("")){
            language = "en";
        }
        //mLogger.message("Speak: \"" + text + "\"");
        if(text.length()> 0) {
            System.out.println("MEssage: " + text);
            String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
            String acoustParams = getAcoustParams("", rawMaryXml, OBADIAH, language);
            ByteArrayOutputStream audio = getAudio("", acoustParams, OBADIAH, language);
            playAudio(audio);
        }
        //this.clearWordList();
    
    }
    
    public void speak(VoiceName speakerVoice, String language) throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        String text = this.getPhrase();
        this.clearWordList();
        //mLogger.message("Speak: \"" + text + "\"");
        if(language == null || language.equals("")){
            language = "en";
        }
        if(text.length()> 0) {
            System.out.println("MEssage: " + text);
            String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
            String acoustParams = getAcoustParams("", rawMaryXml, speakerVoice, language);
            ByteArrayOutputStream audio = getAudio("", acoustParams, speakerVoice,language);
            playAudio(audio);
        }
        //this.clearWordList();
    
    }

    public VoiceName getSpeakingVoice(Gender.TYPE gender, VoiceName voiceName){
        VoiceName speakerVoice = null;
        if(voiceName != null){
            return voiceName;
        }

        if(gender != null && gender == Gender.TYPE.MALE){
            speakerVoice  = I4GMaryClient.OBADIAH;
        }
        else{
            speakerVoice = I4GMaryClient.POPPY;
        }
        return speakerVoice;
    }

    public VoiceName getSpeakingVoice(Gender.TYPE gender){
        VoiceName speakerVoice = null;
        if(gender != null && gender == Gender.TYPE.MALE){
            speakerVoice  = I4GMaryClient.OBADIAH;
        }
        else{
            speakerVoice = I4GMaryClient.POPPY;
        }
        return speakerVoice;
    }

    public void speak(Gender.TYPE gender, String language, VoiceName voiceName) throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        if(language == null || language.equals("")){
            language = "en";
        }
        String text = this.getPhrase();
        this.clearWordList();
        VoiceName speakerVoice = getSpeakingVoice(gender);

        //mLogger.message("Speak: \"" + text + "\"");
        if(text.length()> 0) {
            String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
            String acoustParams = getAcoustParams("", rawMaryXml, speakerVoice,language);
            ByteArrayOutputStream audio = getAudio("", acoustParams, speakerVoice,language);
            playAudio(audio);
        }
        //this.clearWordList();

    }

    public void speak(Gender.TYPE gender, String executionId, VoiceName voiceName, String language) throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        String text = this.getPhrase();
        this.clearWordList();
        VoiceName speakerVoice;
        speakerVoice = getSpeakingVoice(gender, voiceName);
        if(language == null || language.equals("")){
            language = "en";
        }


        //mLogger.message("Speak: \"" + text + "\"");
        if(text.length()> 0) {
            System.out.println("MEssage: " + text);
            String rawMaryXml = getRawMaryXml("", text, languageMap.get(language));
            String acoustParams = getAcoustParams("", rawMaryXml, speakerVoice,language);
            ByteArrayOutputStream audio = getAudio("", acoustParams, speakerVoice,language);
            playAudio(audio, executionId);
        }
        //this.clearWordList();

    }
}

class DataType {

    private String name;

    private DataType(String name) {
        this.name = name;
    }
    public static final DataType TEXT_DE = new DataType("TEXT_DE");
    public static final DataType TEXT_EN = new DataType("TEXT_EN");
    public static final DataType SABLE = new DataType("SABLE");
    public static final DataType SSML = new DataType("SSML");
    public static final DataType RAWMARYXML = new DataType("RAWMARYXML");
    public static final DataType TOKENISED_DE = new DataType("TOKEINZED_DE");
    public static final DataType PREPROCESSED_DE = new DataType("PREPROCESSED_DE");
    public static final DataType CHUNKED_DE = new DataType("CHUNKED_DE");
    public static final DataType PHONEMISED_DE = new DataType("PHONEMISED_DE");
    public static final DataType INTONISED_DE = new DataType("INTONISED_DE");
    public static final DataType POSTPROCESSED_DE = new DataType("POSTPROCESSED_DE");
    public static final DataType ACOUSTPARAMS = new DataType("REALISED_ACOUSTPARAMS");
    public static final DataType MBROLA = new DataType("MBROLA");
    public static final DataType AUDIO = new DataType("AUDIO");

    public String toString() {
        return this.name;
    }
}

class AudioType {

    private String name;

    private AudioType(String name) {
        this.name = name;
    }
    public static final AudioType WAVE = new AudioType("WAVE");
    public static final AudioType AU = new AudioType("AU");
    public static final AudioType SND = new AudioType("SND");
    public static final AudioType AIFF = new AudioType("AIFF");
    public static final AudioType AIFC = new AudioType("AIFC");
    public static final AudioType MP3 = new AudioType("MP3");

    public String toString() {
        return this.name;
    }
}

class Language {

    private String name;

    public Language(String name) {
        this.name = name;
    }

    public String toString() {
        return this.name;
    }
}
