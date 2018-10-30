package de.dfki.vsm.util.tts.cereproc.util;

import com.cereproc.cerevoice_eng.CPRC_VOICE_LOAD_TYPE;
import com.cereproc.cerevoice_eng.SWIGTYPE_p_CPRCEN_engine;
import com.cereproc.cerevoice_eng.cerevoice_eng;

/**
 * Created by alvaro on 7/31/16.
 */
public class CereprocLoader {
    private String voice_name =   "cerevoice_heather_3.2.0_48k.voice";
    private String license_name = "license.lic";
    private SWIGTYPE_p_CPRCEN_engine eng;
    private int chan_handle;

    public CereprocLoader(String voiceN, String licenseN){
        voice_name = voiceN;
        license_name = licenseN;
        try {
            initCereproc();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initCereproc() throws Exception {
        loadVoice();
        openDefaultChannel();
    }


    private void loadVoice() throws Exception {
        eng = cerevoice_eng.CPRCEN_engine_new();
        final int res = cerevoice_eng.CPRCEN_engine_load_voice(eng, license_name, "", voice_name,
                CPRC_VOICE_LOAD_TYPE.CPRC_VOICE_LOAD);
        if (res == 0) {
            throw new Exception("ERROR: unable to load voice file '" + voice_name + "', exiting");
        }
    }

    public Audioline openAudioLine() {
        return getAudioline();
    }

    private Audioline getAudioline() {

        final String rate_str = cerevoice_eng.CPRCEN_channel_get_voice_info(eng, chan_handle, "SAMPLE_RATE");
        final Float rate = new Float(rate_str);
        return new Audioline(rate.floatValue());
    }

    private void openDefaultChannel() {
        chan_handle = cerevoice_eng.CPRCEN_engine_open_default_channel(eng);
    }

    public SWIGTYPE_p_CPRCEN_engine getEng() {
        return eng;
    }

    public int getChan_handle() {
        return chan_handle;
    }
}
