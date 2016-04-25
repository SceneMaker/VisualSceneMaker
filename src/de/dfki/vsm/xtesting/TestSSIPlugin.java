package de.dfki.vsm.xtesting;

import de.dfki.vsm.util.xml.XMLUtilities;
import de.dfki.vsm.xtension.ssi.event.SSIEventArray;
import de.dfki.vsm.xtension.ssi.event.data.SSIStringData;
import de.dfki.vsm.xtension.tworld.TWorldSSIData;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

/**
 * @author Gregor Mehlmann
 */
public class TestSSIPlugin {

    public static void main(String args[]) {

        final String xmldata = "\n"
                + "<ssi>\n"
                + "	<voice>\n"
                + "		<activity>$(event=VoiceActivtiy@audio;field=state;span=0)</activity>\n"
                + "		<keyword>$(event=@speech;span=0)</keyword>\n"
                + "		<praat>\n"
                + "			<pitchmean>$(event=@praat;select=2;span=3000)</pitchmean>\n"
                + "			<pitchsd>$(event=@praat;select=3;span=3000)</pitchsd>\n"
                + "			<speechrate>$(event=@praat;select=30;span=3000)</speechrate>\n"
                + "			<intensity>$(event=@praat;select=34;span=3000)</intensity>\n"
                + "		</praat>\n"
                + "	</voice>\n"
                + "	<head>\n"
                + "		<position>\n"
                + "			<x>$(stream=1;select=0)</x>\n"
                + "			<y>$(stream=1;select=1)</y>\n"
                + "		</position>\n"
                + "		<orientation>\n"
                + "			<roll>$(stream=4;select=0)</roll>\n"
                + "			<pitch>$(stream=4;select=1)</pitch>\n"
                + "			<yaw>$(stream=4;select=2)</yaw>\n"
                + "		</orientation>\n"
                + "		<movement>\n"
                + "			<nod>$(event=@BodyProperties;select=5;span=-1)</nod>\n"
                + "			<shake>$(event=@BodyProperties;select=6;span=-1)</shake>\n"
                + "		</movement>\n"
                + "	</head>\n"
                + "	<body>\n"
                + "		<activity>$(stream=2;select=0)</activity>\n"
                + "		<energy>$(stream=3;select=0)</energy>\n"
                + "		<posture>\n"
                + "			<leanfront>\n"
                + "				<detected>$(event=LeanFront@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=LeanFront@Fubi;field=duration)</duration>\n"
                + "				<intensity>$(event=@BodyProperties;select=0;span=-1)</intensity>\n"
                + "			</leanfront>\n"
                + "			<leanback>\n"
                + "				<detected>$(event=LeanBack@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=LeanBack@Fubi;field=duration)</duration>\n"
                + "				<intensity>$(event=@BodyProperties;select=0;span=-1)</intensity>\n"
                + "			</leanback>\n"
                + "		</posture>\n"
                + "		<gesture>\n"
                + "			<armsopen>\n"
                + "				<detected>$(event=ArmsOpen@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=ArmsOpen@Fubi;field=duration)</duration>\n"
                + "				<intensity>$(event=@BodyProperties;select=1;span=-1)</intensity>\n"
                + "			</armsopen>\n"
                + "			<armscrossed>\n"
                + "				<detected>$(event=ArmsCrossed@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=ArmsCrossed@Fubi;field=duration)</duration>\n"
                + "				<intensity>$(event=@BodyProperties;select=4;span=-1)</intensity>\n"
                + "			</armscrossed>\n"
                + "			<lefthandheadtouch>\n"
                + "				<detected>$(event=LeftHandHeadTouch@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=LeftHandHeadTouch@Fubi;field=duration)</duration>\n"
                + "			</lefthandheadtouch>\n"
                + "			<righthandheadtouch>\n"
                + "				<detected>$(event=RightHandHeadTouch@Fubi;field=state;span=-1)</detected>\n"
                + "				<duration>$(event=RightHandHeadTouch@Fubi;field=duration)</duration>\n"
                + "			</righthandheadtouch>\n"
                + "		</gesture>\n"
                + "	</body>\n"
                + "	<face>\n"
                + "		<expression>\n"
                + "			<smile>\n"
                + "                <detected>$(event=smile@facialexpression;field=state;span=-1)</detected>\n"
                + "                <duration>$(event=smile@facialexpression;field=duration)</duration>\n"
                + "                <intensity>$(stream=0;select=0)</intensity>\n"
                + "			</smile>\n"
                + "		</expression>\n"
                + "	</face>\n"
                + "</ssi>\n";

        final String ssievents = ""
                + "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                + "<events ssi-v=\"V2\">\n"
                + "    <event\n"
                + "        sender=\"xml\"\n"
                + "        event=\"xml\"\n"
                + "        from=\"0\"\n"
                + "        dur=\"0\"\n"
                + "        prob=\"1.000000\"\n"
                + "        type=\"STRING\"\n"
                + "        state=\"COMPLETED\"\n"
                + "        glue=\"0\">"
                + "<![CDATA["
                + xmldata
                + "]]>"
                + "</event>\n"
                + "</events>\n";

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
            //System.out.print(out.toString());
        } catch (final UnsupportedEncodingException exc) {
            exc.printStackTrace();
        }

        final TWorldSSIData mSSIData = new TWorldSSIData(
                ((SSIStringData) array.getEventList().get(0).getData()).toString());

        System.out.println(mSSIData.get("voice.activity"));
        System.out.println(mSSIData.get("voice.keyword"));
        System.out.println(mSSIData.get("voice.praat.pitchmean"));
        System.out.println(mSSIData.get("voice.praat.pitchsd"));
        System.out.println(mSSIData.get("voice.praat.speechrate"));
        System.out.println(mSSIData.get("voice.praat.intensity"));
        System.out.println(mSSIData.get("head.position.x"));
        System.out.println(mSSIData.get("head.position.y"));
        System.out.println(mSSIData.get("head.orientation.roll"));
        System.out.println(mSSIData.get("head.orientation.pitch"));
        System.out.println(mSSIData.get("head.orientation.yaw"));
        System.out.println(mSSIData.get("head.movement.nod"));
        System.out.println(mSSIData.get("head.movement.shake"));
        System.out.println(mSSIData.get("body.activity"));
        System.out.println(mSSIData.get("body.energy"));
        System.out.println(mSSIData.get("body.posture.leanfront.detected"));
        System.out.println(mSSIData.get("body.posture.leanfront.duration"));
        System.out.println(mSSIData.get("body.posture.leanfront.intensity"));
        System.out.println(mSSIData.get("body.posture.leanback.detected"));
        System.out.println(mSSIData.get("body.posture.leanback.duration"));
        System.out.println(mSSIData.get("body.posture.leanback.intensity"));
        System.out.println(mSSIData.get("body.gesture.armsopen.detected"));
        System.out.println(mSSIData.get("body.gesture.armsopen.duration"));
        System.out.println(mSSIData.get("body.gesture.armsopen.intensity"));
        System.out.println(mSSIData.get("body.gesture.armscrossed.detected"));
        System.out.println(mSSIData.get("body.gesture.armscrossed.duration"));
        System.out.println(mSSIData.get("body.gesture.armscrossed.intensity"));
        System.out.println(mSSIData.get("body.gesture.lefthandheadtouch.detected"));
        System.out.println(mSSIData.get("body.gesture.lefthandheadtouch.duration"));
        System.out.println(mSSIData.get("body.gesture.righthandheadtouch.detected"));
        System.out.println(mSSIData.get("body.gesture.righthandheadtouch.duration"));
        System.out.println(mSSIData.get("face.expression.smile.detected"));
        System.out.println(mSSIData.get("face.expression.smile.duration"));
        System.out.println(mSSIData.get("face.expression.smile.intensity"));

    }
}
