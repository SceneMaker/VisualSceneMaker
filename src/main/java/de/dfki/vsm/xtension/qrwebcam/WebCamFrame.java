package de.dfki.vsm.xtension.qrwebcam;

import com.github.sarxos.webcam.Webcam;
import com.github.sarxos.webcam.WebcamPanel;
import com.github.sarxos.webcam.WebcamResolution;
import com.google.zxing.BinaryBitmap;
import com.google.zxing.LuminanceSource;
import com.google.zxing.MultiFormatReader;
import com.google.zxing.NotFoundException;
import com.google.zxing.Result;
import com.google.zxing.client.j2se.BufferedImageLuminanceSource;
import com.google.zxing.common.HybridBinarizer;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.awt.FlowLayout;
import java.awt.image.BufferedImage;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

/**
 * @author Patrick Gebhard
 */
public class WebCamFrame extends JFrame {

    private static Webcam sWebcam = null;
    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    private static WebCamFrame sInstance = new WebCamFrame();
    private WebcamPanel mPanel = null;

    public static WebCamFrame getInstance() {
        sInstance = (sInstance != null) ? sInstance : new WebCamFrame();
        return sInstance;
    }

    private WebCamFrame() {
        setLayout(new FlowLayout());

        sWebcam = Webcam.getDefault();
        sWebcam.setViewSize(WebcamResolution.VGA.getSize());
        mPanel = new WebcamPanel(sWebcam, false);
        add(mPanel);

        setTitle("Cam");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        pack();
        setVisible(true);

        SwingUtilities.invokeLater(() -> mPanel.start());

    }

    public void shutdown() {
        sWebcam.removeWebcamListener(mPanel);
        if (sWebcam.isOpen()) {
            sWebcam.close();
        }
        mPanel.stop();
        dispose();
        sWebcam = null;
        sInstance = null;
    }

    public String getQRCode() {
        String decodedText = "";
        BufferedImage image = sWebcam.getImage();

        if (image != null) {
            try {
                decodedText = decodeQRCode(image);
                if (decodedText == null) {
                    decodedText = "";
                } else {
                    mLogger.message("Detected code " + decodedText);
                    setTitle("Cam Window - Detected Code " + decodedText);
                }
            } catch (IOException e) {
                mLogger.warning("Directly executing activity at timemark " + e.getMessage());
            }
        } else {
            decodedText = "";
        }

        return decodedText;
    }

    private static String decodeQRCode(BufferedImage bufferedImage) throws IOException {
        LuminanceSource source = new BufferedImageLuminanceSource(bufferedImage);
        BinaryBitmap bitmap = new BinaryBitmap(new HybridBinarizer(source));

        try {
            Result result = new MultiFormatReader().decode(bitmap);
            return result.getText();
        } catch (NotFoundException e) {
            return null;
        }
    }
}
