package de.dfki.vsm.util.tts.cereproc.util;

import java.io.File;

/**
 * Created by alvaro on 1/15/17.
 */
public class CereprocLibPath {
    public static String cerevoiceLibPath;
    public static String getCereprocLibraryPath() {
        String libPath = cerevoiceLibPath;
        File filePath = new File(libPath);
        String javalibPath = "/home/alvaro/Documents/Universitat/TesisProject/cerevoice_sdk_3.2.0_linux_x86_64_python26_10980_academic/cerevoice_eng/javalib";
        if(libPath.length() > 0 && filePath.exists()){
            javalibPath = libPath;
        }
        return javalibPath;
    }
}
