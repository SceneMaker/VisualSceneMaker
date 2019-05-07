package de.dfki.vsm.xtension.fortunecookie;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Properties;

/**
 * @author Patrick Gebhard
 */
public final class FortuneCookieExecutor extends ActivityExecutor {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
    // The fortune cookie mapping properties
    private final Properties mNumberCookieMapping = new Properties();
    // The fortune cookie user mapping properties
    private final Properties mUserCookieMapping = new Properties();

    // Construct executor
    public FortuneCookieExecutor(final PluginConfig config, final RunTimeProject project) {
        super(config, project);
    }

    // Get marker syntax
    @Override
    public synchronized String marker(final long id) {
        return "$(" + id + ")";
    }

    @Override
    public final void launch() {
        mLogger.message("Launching FortuneCookieExecutor ...");

        // load fortune cookie database
        try {
            String wmf = mProject.getProjectPath() + File.separator + mConfig.getProperty("cookies");
            wmf = wmf.replace("\\", "/");
            mNumberCookieMapping.load(new FileReader(new File(wmf)));
            mLogger.message(mNumberCookieMapping.size() + " fortune cookies found");
        } catch (IOException ex) {
            mLogger.failure("fortune cookie file (" + mProject.getProjectPath() + File.separator + mConfig.getProperty("cookies") + ") not found!");
        }

        // load fortune cookie users
        try {
            String wmf = mProject.getProjectPath() + File.separator + mConfig.getProperty("users");
            wmf = wmf.replace("\\", "/");
            mUserCookieMapping.load(new FileReader(new File(wmf)));
            mLogger.message(mUserCookieMapping.size() + " fortune cookies users found");
        } catch (IOException ex) {
            mLogger.failure("fortune cookie user file (" + mProject.getProjectPath() + File.separator + mConfig.getProperty("cookies") + ") not found!");
        }

    }

    @Override
    public final void unload() {
        mLogger.message("Stopping FortuneCookieExecutor ...");
    }

    @Override
    public void execute(final AbstractActivity activity) {
        // Get action information
        final AbstractActivity.Type activity_type = activity.getType();
        final String activity_text = activity.getText();
        final String activity_name = activity.getName();
        //final String activity_mode = activity.getMode();
        final String activity_actor = activity.getActor();
        final List activity_features = activity.getFeatures();

        //activity.setType(AbstractActivity.Type.blocking);
        // Get log message features
        final String name = activity.getName();
        if (name.equalsIgnoreCase("getCookie")) {

            //mProject.setVariable("code", code);
        }
    }
}
