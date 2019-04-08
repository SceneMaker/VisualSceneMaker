package de.dfki.vsm.xtension.qrwebcam;

public class NoWebcamException extends Throwable {
    @Override
    public String getMessage() {
        return "no webcam detected";
    }
}
