package de.dfki.vsm.xtension.qrwebcam;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;

/**
 * @author Patrick Gebhard
 */
public final class QRCodeGrabber extends ActivityExecutor {

    private static WebCamFrame mWebcamwindow = null;
        // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    
    
    // Construct executor
    public QRCodeGrabber(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public final void launch() {
        mLogger.message("Launching QRCodeGrabber ...");
        mWebcamwindow = WebCamFrame.getInstance();
         mWebcamwindow.setVisible(true);
    }

    @Override
    public final void unload() {
        mWebcamwindow.setVisible(false);
    }

    @Override
    public void execute(final AbstractActivity activity) {

        //activity.setType(AbstractActivity.Type.blocking);
        // Get log message features
        final String name = activity.getName();
        if (name.equalsIgnoreCase("grab")) {
            String code = (mWebcamwindow!= null) ? mWebcamwindow.getQRCode(): "";
            mProject.setVariable("code", code);
        }
    }
}
