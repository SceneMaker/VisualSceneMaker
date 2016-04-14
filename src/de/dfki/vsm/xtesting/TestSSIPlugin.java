package de.dfki.vsm.xtesting;

import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.SSIRunTimePlugin;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.data.SSIXMLData;
import de.dfki.vsm.xtension.tworld.TWorldSSIData;
import de.dfki.vsm.xtension.tworld.TWorldSSIData.VoiceData;
import de.dfki.vsm.xtension.tworld.TWorldSSIPlugin;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;

/**
 * @author Gregor Mehlmann
 */
public class TestSSIPlugin {

    public static void main(String args[]) {

        
        
        
         try {
                //final RunTimeProject project = new RunTimeProject();
                // Get the class object
                final Class clazz = Class.forName("de.dfki.vsm.xtension.tworld.TWorldSSIPlugin");
                // Get the constructor
                final Constructor constructor
                        = clazz.getConstructor(PluginConfig.class, RunTimeProject.class);
                // Call the constructor
                final RunTimePlugin plugin = (RunTimePlugin) constructor.newInstance(null, null);
                //
                plugin.launch();
                
            } catch (final Exception exc) {
               exc.printStackTrace();
            }
        
     
        /*
        
        final String xmldata = ""
                // + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<ssi>\n"
                + "    <voice>\n"
                + "        <activity>0</activity>\n"
                + "        <keyword>goodbye</keyword>\n"
                + "        <praat>\n"
                + "            <PitchMean>140.964005</PitchMean>\n"
                + "            <PitchSD>54.827000</PitchSD>\n"
                + "            <SpeechRate>1.875000</SpeechRate>\n"
                + "            <Intensity>65.965355</Intensity>\n"
                + "        </praat>\n"
                + "    </voice>\n"
                + "    <head>\n"
                + "        <pos>\n"
                + "            <x>203.931671</x>\n"
                + "            <y>300.109741</y>\n"
                + "        </pos>\n"
                + "        <move>\n"
                + "            <nod>1</nod>\n"
                + "            <shake>0</shake>\n"
                + "        </move>\n"
                + "    </head>\n"
                + "    <body>\n"
                + "        <openness>0.7</openness>\n"
                + "        <activity>20.4</activity>\n"
                + "        <energy>10.4</energy>\n"
                + "        <leans>\n"
                + "            <lean>\n"
                + "                <identifier>front</identifier>\n"
                + "                <detected>1</detected>\n"
                + "                <duration>1</duration>\n"
                + "                <intensity>0.2</intensity>\n"
                + "            </lean>\n"
                + "            <lean>\n"
                + "                <identifier>back</identifier>\n"
                + "                <detected>0</detected>\n"
                + "                <duration>1</duration>\n"
                + "                <intensity>0.0</intensity>\n"
                + "            </lean>\n"
                + "        </leans>\n"
                + "        <gests>\n"
                + "            <gest>\n"
                + "                <identifier>armsopen</identifier>\n"
                + "                <detected>1</detected>\n"
                + "                <duration>1505</duration>\n"
                + "                <intensity>1.0</intensity>\n"
                + "            </gest>\n"
                + "            <gest>\n"
                + "                <identifier>armscrossed</identifier>\n"
                + "                <detected>0</detected>\n"
                + "                <duration>0</duration>\n"
                + "                <intensity>0.0</intensity>\n"
                + "            </gest>\n"
                + "        </gests>\n"
                + "    </body>\n"
                + "    <face>\n"
                + "        <exps>\n"
                + "            <exp>\n"
                + "                <identifier>smile</identifier>\n"
                + "                <detected>1</detected>\n"
                + "                <duration>1000</duration>\n"
                + "                <intensity>0.968421</intensity>\n"
                + "            </exp>\n"
                + "            <exp>\n"
                + "                <identifier>frown</identifier>\n"
                + "                <detected>0</detected>\n"
                + "                <duration>0</duration>\n"
                + "                <intensity>0.0</intensity>\n"
                + "            </exp>\n"
                + "        </exps>\n"
                + "    </face>\n"
                + "</ssi>\n";

        final String ssievents = ""
                + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<events ssi-v=\"V2\">\n"
                + "    <event\n"
                + "        sender=\"xmlevent\"\n"
                + "        event=\"xml\"\n"
                + "        from=\"0\"\n"
                + "        dur=\"0\"\n"
                + "        prob=\"1.000000\"\n"
                + "        type=\"XML\"\n"
                + "        state=\"COMPLETED\"\n"
                + "        glue=\"0\">"
                + "<![CDATA["
                + xmldata
                + "]]>"
                + "    </event>\n"
                + "</events>\n";

        System.out.print(ssievents);

        final SSIEventArray array = new SSIEventArray();
        try {
            //
            final ByteArrayInputStream in = new ByteArrayInputStream(
                    ssievents.getBytes("UTF-8"));
            final ByteArrayOutputStream out = new ByteArrayOutputStream();
            //
            XMLUtilities.parseFromXMLStream(array, in);
            XMLUtilities.writeToXMLStream(array, out);
            //
            System.out.print(out.toString());
        } catch (final UnsupportedEncodingException exc) {
            exc.printStackTrace();
        }

        final TWorldSSIData mSSIData = new TWorldSSIData(
                ((SSIXMLData) array.getEventList().get(0).getData()).getXML());

        System.out.println(mSSIData.getVoiceData().getActivity());
        System.out.println(mSSIData.getVoiceData().getKeyword());
        System.out.println(mSSIData.getVoiceData().getPraatData().getSpeechRate());
        System.out.println(mSSIData.getVoiceData().getPraatData().getIntensity());
        System.out.println(mSSIData.getVoiceData().getPraatData().getPitchMean());
        System.out.println(mSSIData.getVoiceData().getPraatData().getPitchSD());

        //  System.out.println(mSSIData.getHeadData().getNodData());
        //  System.out.println(mSSIData.getHeadData().getShakeData());
        System.out.println(mSSIData.getHeadData().getPosData().getX());
        System.out.println(mSSIData.getHeadData().getPosData().getY());

        System.out.println(mSSIData.getBodyData().getActivity());
        System.out.println(mSSIData.getBodyData().getOpeness());
        System.out.println(mSSIData.getBodyData().getEnergy());
        System.out.println(mSSIData.getBodyData().getLeanData("front").getIdentifier());
        System.out.println(mSSIData.getBodyData().getLeanData("front").getDetected());
        System.out.println(mSSIData.getBodyData().getLeanData("front").getDuration());
        System.out.println(mSSIData.getBodyData().getLeanData("front").getIntensity());
        System.out.println(mSSIData.getBodyData().getLeanData("back").getIdentifier());
        System.out.println(mSSIData.getBodyData().getLeanData("back").getDetected());
        System.out.println(mSSIData.getBodyData().getLeanData("back").getDuration());
        System.out.println(mSSIData.getBodyData().getLeanData("back").getIntensity());
        System.out.println(mSSIData.getBodyData().getGestData("armsopen").getIdentifier());
        System.out.println(mSSIData.getBodyData().getGestData("armsopen").getDetected());
        System.out.println(mSSIData.getBodyData().getGestData("armsopen").getDuration());
        System.out.println(mSSIData.getBodyData().getGestData("armsopen").getIntensity());
        System.out.println(mSSIData.getBodyData().getGestData("armscrossed").getIdentifier());
        System.out.println(mSSIData.getBodyData().getGestData("armscrossed").getDetected());
        System.out.println(mSSIData.getBodyData().getGestData("armscrossed").getDuration());
        System.out.println(mSSIData.getBodyData().getGestData("armscrossed").getIntensity());

        System.out.println(mSSIData.getFaceData().getExpData("smile").getIdentifier());
        System.out.println(mSSIData.getFaceData().getExpData("smile").getDetected());
        System.out.println(mSSIData.getFaceData().getExpData("smile").getDuration());
        System.out.println(mSSIData.getFaceData().getExpData("smile").getIntensity());
        System.out.println(mSSIData.getFaceData().getExpData("frown").getIdentifier());
        System.out.println(mSSIData.getFaceData().getExpData("frown").getDetected());
        System.out.println(mSSIData.getFaceData().getExpData("frown").getDuration());
        System.out.println(mSSIData.getFaceData().getExpData("frown").getIntensity());
        */
    }
}
