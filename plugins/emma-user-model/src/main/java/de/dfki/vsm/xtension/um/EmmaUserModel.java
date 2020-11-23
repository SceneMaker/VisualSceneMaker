package de.dfki.vsm.xtension.um;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.*;
import java.util.Calendar;
import java.util.List;


/**
 * Created by Patrick
 */
public class EmmaUserModel extends ActivityExecutor {

    // List of all users
    private JSONObject mUserProfiles = new JSONObject();

    // current user
    private JSONObject mUser = null;

    // The singleton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    public EmmaUserModel(PluginConfig config, RunTimeProject project) {
        super(config, project);
    }

    @Override
    public final synchronized String marker(final long id) {
        // Bracket style bookmarks
        return "$(" + id + ")";
    }

    @Override
    public void launch() {
        mLogger.message("Loading EmmA UserModel Executor ...");

        // load user profiles
        loadUserModel();
    }

    @Override
    public void execute(AbstractActivity activity) {
        // Get action information
        final Type activity_type = activity.getType();
        final String activity_text = activity.getText();
        final String activity_name = activity.getName();
        //final String activity_mode = activity.getMode();
        final String activity_actor = activity.getActor();
        final List activity_features = activity.getFeatures();
        // set all activities blocking
        activity.setType(Type.blocking);

        final String name = activity.getName();
        if (name.equalsIgnoreCase("set")) {
            setUserValue("break", activity);
            setUserValue("type", activity);
            setUserValue("therapy", activity);
            setUserValue("icd", activity);
            setUserValue("contact", activity);
            setUserValue("contactphone", activity);
            setUserValue("therapist", activity);
            setUserValue("therapistphone", activity);
            setUserValue("nextworktime_mo", activity);
            setUserValue("nextworktime_tu", activity);
            setUserValue("nextworktime_we", activity);
            setUserValue("nextworktime_th", activity);
            setUserValue("nextworktime_fr", activity);
            setUserValue("actworktime_mo", activity);
            setUserValue("actworktime_tu", activity);
            setUserValue("actworktime_we", activity);
            setUserValue("actworktime_th", activity);
            setUserValue("actworktime_fr", activity);
            setUserValue("pos_activitiy", activity);

            // add/update to user profiles
            saveUserModel();
        }

        if (name.equalsIgnoreCase("get")) {
            if ((activity.get("strVar") != null) && (mProject.hasVariable(activity.get("strVar")))) {
                String val = getUserStrValue(activity);
                mProject.setVariable(activity.get("strVar"), new StringValue(val));
            }
            if ((activity.get("intVar") != null) && (mProject.hasVariable(activity.get("intVar")))) {
                Integer val = getUserIntValue(activity);
                mProject.setVariable(activity.get("intVar"), new IntValue(val));
            }
        }

        // load creates a new user model, if there is no user with the specific name.
        if (name.equalsIgnoreCase("load")) {
            if ((activity.get("name") != null)) {
                String userName = activity.get("name");
                mUser = loadUserData(userName);

                if (mUser != null) {
                    mLogger.message("Data from user " + userName + " found!");
                } else {
                    JSONArray users = mUserProfiles.getJSONArray("users");
                    JSONObject newUser = createUser(userName, users.length() + 1);
                    users.put(newUser);

                    saveUserModel();

                    mLogger.warning("Data from user " + userName + " not found - new user created!");
                }
            }
        }

        if (name.equalsIgnoreCase("diary")) {
            if (mUser != null) {
                JSONObject diaryentry = new JSONObject();

                diaryentry.put("date", Calendar.getInstance().getTime());
                diaryentry.put("no", getLastDiaryEntryNumber() + 1);
                diaryentry.put("producer", (activity.get("producer") != null) ? activity.get("producer") : "");
                diaryentry.put("entry", (activity.get("entry") != null) ? activity.get("entry") : "");
            } else {
                mLogger.warning("No user specified, diary entry will not be stored.");
            }
        }
    }

    // set also overrides previously set values.
    private void setUserValue(String key, AbstractActivity activity) {
        if ((activity.get(key) != null) && (mUser.get(key) != "")) {
            mUser.putOnce(key, activity.get(key));
        }
    }

    // get String value.
    private String getUserStrValue(AbstractActivity activity) {
        final String key = "value";
        if ((activity.get(key) != null) && (mUser.get(activity.get(key)) != "")) {
            return mUser.getString(activity.get(key));
        } else {
            return "";
        }
    }

    // get Integer value.
    private Integer getUserIntValue(AbstractActivity activity) {
        final String key = "value";
        if ((activity.get(key) != null) && (mUser.get(activity.get(key)) != "")) {
            return Integer.parseInt(mUser.getString(activity.get(key)));
        } else {
            return -1;
        }
    }

    private long getLastDiaryEntryNumber() {
        JSONArray diary = mUser.getJSONArray("diary");
        long biggestNo = 1;

        for (int i = 0; i < diary.length(); i++) {
            JSONObject item = diary.getJSONObject(i);
            biggestNo = (item.getLong("no") > biggestNo) ? item.getLong("no") : biggestNo;
        }

        return biggestNo;
    }

    private JSONObject loadUserData(String name) {
        JSONArray users = mUserProfiles.getJSONArray("users");
        for (int i = 0; i < users.length(); i++)  {
            JSONObject item = users.getJSONObject(i);
            if (item.getString("name").equalsIgnoreCase(name)) return item;
        }
        return null;
    }

    private JSONObject createUser(String name, long id) {
        JSONObject user = new JSONObject();

        // initial user data
        user.put("name", name);
        user.put("id", id);
        user.put("break", "unknown");
        user.put("type", "unknown");
        user.put("therapy", "unknown");
        user.put("icd", "unknown");
        user.put("contact", "unknown");
        user.put("contactphone", "unknown");
        user.put("therapist", "unknown");
        user.put("therapistphone", "unknown");
        user.put("nextworktime_mo", "unknown");
        user.put("nextworktime_tu", "unknown");
        user.put("nextworktime_we", "unknown");
        user.put("nextworktime_th", "unknown");
        user.put("nextworktime_fr", "unknown");
        user.put("actworktime_mo", "unknown");
        user.put("actworktime_tu", "unknown");
        user.put("actworktime_we", "unknown");
        user.put("actworktime_th", "unknown");
        user.put("actworktime_fr", "unknown");
        user.put("posactivity", "unknown");

        // intial diary entry
        JSONArray diary = new JSONArray();
        JSONObject diaryentry = new JSONObject();

        diaryentry.put("date", Calendar.getInstance().getTime());
        diaryentry.put("no", 1);
        diaryentry.put("producer", "system");
        diaryentry.put("entry", "created");

        diary.put(diaryentry);
        user.put("diary", diary);

        return user;
    }

    private void loadUserModel() {
        mLogger.message("Loading EmmA User Model ...");
        String umf = (mProject.getProjectPath() + File.separator + mConfig.getProperty("umdir") + File.separator + "UM.json").replace("\\", "/");

        String input = "";
        try {
            BufferedReader br = new BufferedReader(new FileReader(umf));
            StringBuilder sb = new StringBuilder();
            String line = br.readLine();
            while (line != null) {
                sb.append(line);
                line = br.readLine();
            }
            input = sb.toString();
        } catch (FileNotFoundException e) {
            mLogger.warning("No User Model found in " + umf + ", creating new.");

            // create first entry, with the first user - id 0
            //out object and user array object
            JSONObject jsonOut = new JSONObject();
            JSONArray jsonUserArray = new JSONArray();

            // user
            jsonUserArray.put(createUser("unknown", 0));
            jsonOut.put("users", jsonUserArray);

            input = jsonOut.toString();
        } catch (Exception e) {
            e.printStackTrace();
        }

        mUserProfiles = new JSONObject(input);

        saveUserModel();

        JSONArray users = mUserProfiles.getJSONArray("users");

        mLogger.message("Found user " + users.get(0).toString());
    }

    private void saveUserModel() {
        String umf = mProject.getProjectPath() + File.separator + mConfig.getProperty("umdir") + File.separator + "UM.json";
        try {
            FileWriter umfw = new FileWriter(umf);

            mLogger.message("Saving user profiles " + mUserProfiles);

            umfw.write(mUserProfiles.toString());
            umfw.flush();
            umfw.close();
        } catch (IOException e) {
            mLogger.failure("Error writing UM to " + umf);
        }
    }

    @Override
    public void unload() {
        // save user data in user Model
        saveUserModel();
    }
}
