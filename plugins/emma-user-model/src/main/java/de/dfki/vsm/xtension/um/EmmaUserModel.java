package de.dfki.vsm.xtension.um;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.*;
import java.util.Calendar;
import java.util.Date;
import java.util.List;


/**
 * Created by Patrick
 */
public class EmmaUserModel extends ActivityExecutor {

    // List of all users
    private JSONObject mUserProfiles = new JSONObject();

    // current user
    private JSONObject mUser = null;

    // The singelton logger instance
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
        mLogger.message("Loading UMExecutor ...");

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
            mLogger.message("Emma User Model setting user attributes ...");

//            String breakLength = (activity.get("break") != null) ? activity.get("break") : "";
//            String breakType = (activity.get("type") != null) ? activity.get("type") : "";
//            String inTherapy = (activity.get("therapy") != null) ? activity.get("therapy") : "";
//            String icdCode = (activity.get("icd") != null) ? activity.get("therapy") : "";
//            String contact = (activity.get("contact") != null) ? activity.get("contact") : "";
//            String contactPhone = (activity.get("contactphone") != null) ? activity.get("contactphone") : "";
//            String therapist = (activity.get("therapist") != null) ? activity.get("therapist") : "";
//            String therapistPhone = (activity.get("therapistphone") != null) ? activity.get("therapistphone") : "";
//            String planedWorkTimeMo = (activity.get("nextworktime_mo") != null) ? activity.get("nextworktime_mo") : "";
//            String planedWorkTimeTue = (activity.get("nextworktime_mo") != null) ? activity.get("nextworktime_tu") : "";
//            String planedWorkTimeWed = (activity.get("nextworktime_we") != null) ? activity.get("nextworktime_we") : "";
//            String planedWorkTimeThr = (activity.get("nextworktime_th") != null) ? activity.get("nextworktime_th") : "";
//            String planedWorkTimeFri = (activity.get("nextworktime_fr") != null) ? activity.get("nextworktime_fr") : "";
//            String actualWorkTimeMo = (activity.get("actworktime_mo") != null) ? activity.get("actworktime_mo") : "";
//            String actualWorkTimeTue = (activity.get("actworktime_tu") != null) ? activity.get("actworktime_tu") : "";
//            String actualWorkTimeWed = (activity.get("actworktime_we") != null) ? activity.get("actworktime_we") : "";
//            String actualWorkTimeThr = (activity.get("actworktime_th") != null) ? activity.get("actworktime_th") : "";
//            String actualWorkTimeFri = (activity.get("actworktime_fr") != null) ? activity.get("actworktime_fr") : "";
//            String positiveActivitiy = (activity.get("pos_activitiy") != null) ? activity.get("pos_activitiy") : "";
//
//            // add/update to user profiles
//
//            saveUserModel();
        }

        if (name.equalsIgnoreCase("load")) {
            if ((activity.get("name") != null)) {
                String userName =  activity.get("name");
                mUser = loadUserData(userName);

                if (mUser != null) {
                    mLogger.message("Data from user " + userName + " found!");
                } else {
                    JSONObject newUser = createUser(userName);
                    JSONArray users = mUserProfiles.getJSONArray("users");
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

    private long getLastDiaryEntryNumber() {
        JSONArray diary = mUser.getJSONArray("diary");
        long biggestNo = 1;

        for (int i = 0; i < diary.length(); i++)  {
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

    private JSONObject createUser(String name) {
        JSONObject user = new JSONObject();

        // initial user data
        user.put("name", name);
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
        String umf = (mProject.getProjectPath() + File.separator + mConfig.getProperty("umdir") + File.separator + "UM.json").replace("\\", "/");;

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
            jsonUserArray.put(createUser("unknown"));
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
