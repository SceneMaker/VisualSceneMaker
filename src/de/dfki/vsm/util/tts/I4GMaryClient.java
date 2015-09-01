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
package de.dfki.vsm.util.tts;


import de.dfki.vsm.util.log.LOGDefaultLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.UnsupportedAudioFileException;

import javax.xml.parsers.ParserConfigurationException;
import marytts.util.http.Address;



import marytts.client.MaryClient;
import marytts.util.data.audio.AudioPlayer;

import marytts.util.data.audio.AudioPlayer;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.xml.sax.SAXException;


public class I4GMaryClient {

    private MaryClient maryClient = null;
    public static final Language Language_DE = new Language("de");
    public static final Language Language_EN = new Language("en");
    public static final VoiceName FEMALE = new VoiceName("female");
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
    public static final VoiceName CMU = new VoiceName("cmu-slt-hsmm");
    private static final String SAMPLE_TEXT = "Willkommen in der Welt der Sprachsynthese!";
    // Singleton
    static private I4GMaryClient instance = null;
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
    private List wordQueue = null;
    private String speak_text = "";


    private I4GMaryClient() throws IOException{
        
        this(System.getProperty("server.host", "localhost"), Integer.getInteger("server.port", 59125).intValue());
       // wordQueue = new LinkedList<>();
        wordQueue =  Collections.synchronizedList(new LinkedList());
        
    }

    private I4GMaryClient(String host, int port) throws IOException{
        

        Address address = new Address(host, port);
        this.maryClient = maryClient.getMaryClient(address);

        
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

    public void playAudio(AudioInputStream ais) {
        LineListener lineListener = new LineListener() {

            public void update(LineEvent event) {
                if (event.getType() == LineEvent.Type.START) {
                   
                } else if (event.getType() == LineEvent.Type.STOP) {
                    mLogger.message("Audio stopped playing.");
                } else if (event.getType() == LineEvent.Type.OPEN) {
                    mLogger.message("Audio line opened.");
                } else if (event.getType() == LineEvent.Type.CLOSE) {
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

    public String getAcoustParams(String speaker, String rawMaryXml, VoiceName voice) throws IOException, UnknownHostException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // emotional processing ... set new voices and effects
        String voiceName = voice.toString();
        
        String effects = "";

        // process (old version)
        //maryClient.process(rawMaryXml, DataType.RAWMARYXML.toString(), DataType.ACOUSTPARAMS.toString(), null, voice.toString(), baos); // ggf. AudioType.WAVE
        maryClient.process(rawMaryXml, "RAWMARYXML", "ACOUSTPARAMS", "de",  "WAVE", voiceName,  baos); // ggf. AudioType.WAVE
         
        String acoustParams = baos.toString();
        baos.close();
        return acoustParams;
    }
    
    public ByteArrayOutputStream getAudio(String speaker, String rawMaryXml, VoiceName voice) throws IOException, UnknownHostException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        // emotional processing ... set new voices and effects
        String voiceName = voice.toString();
        String effects = "";

        
        //maryClient.process(rawMaryXml, DataType.RAWMARYXML.toString(), DataType.AUDIO.toString(), AudioType.WAVE.toString(), voice.toString(), baos);
          maryClient.process(rawMaryXml, "ACOUSTPARAMS", "AUDIO", "de",  "WAVE", voiceName,  baos); // ggf. AudioType.WAVE
        
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
    
    private String getPhrase(){
        String finalWord = "";
        synchronized(wordQueue){
            Iterator it = wordQueue.iterator();
            while (it.hasNext()) {
                String word = (String) it.next();
                finalWord+= word + " ";
            }
        }
        
        mLogger.message("Word to process: \"" + finalWord + "\"");
        return finalWord;
    }
    
    public long getPhraseTime() throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception { //TODO: Get language from scene
        String text = this.getPhrase();
        String rawMaryXml = getRawMaryXml("", text, Language_DE);
        String acoustParams = getAcoustParams("", rawMaryXml, BITS3);
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
    
    public void clearWordList(){
        synchronized(wordQueue){
            wordQueue.clear();
        }
        
    }
    
    public void speak(String text)
            throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        /*MaryInterface marytts = new LocalMaryInterface();
        Set<String> voices = marytts.getAvailableVoices();*/
        
        mLogger.message("Speak: \"" + text + "\"");
        String rawMaryXml = getRawMaryXml("", text, Language_DE);
        String acoustParams = getAcoustParams("", rawMaryXml, BITS3);
        ByteArrayOutputStream audio = getAudio("", acoustParams, BITS3);
        playAudio(audio);
        //this.clearWordList();
    }
    
    public void speak() throws IOException, UnknownHostException, UnsupportedAudioFileException,
            InterruptedException, Exception {
        String text = this.getPhrase();
        this.clearWordList();
        mLogger.message("Speak: \"" + text + "\"");
        String rawMaryXml = getRawMaryXml("", text, Language_DE);
        String acoustParams = getAcoustParams("", rawMaryXml, BITS3);
        ByteArrayOutputStream audio = getAudio("", acoustParams, BITS3);
        playAudio(audio);
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

class VoiceName {

    private String name;

    public VoiceName(String name) {
        this.name = name;
    }

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
