package de.dfki.vsm.xtension.um;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.activity.AbstractActivity;
import de.dfki.vsm.runtime.activity.AbstractActivity.Type;
import de.dfki.vsm.runtime.activity.executor.ActivityExecutor;
import de.dfki.vsm.runtime.interpreter.value.IntValue;
import de.dfki.vsm.runtime.interpreter.value.StringValue;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGDefaultLogger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.*;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;


/**
 * Created by Patrick,
 * Adapted by Chirag Bhuvaneshwara
 */
public class EmmaUserModel extends ActivityExecutor {

    // List of all users
    private JSONObject mUserProfiles = new JSONObject();
    // current user
    private JSONObject mUser = null;
    // days at which the user had a diary conversation
    private List<String> mDiaryDays = new LinkedList<>();
    // current day, daily items, and context
    private int mSelectedDay = -1;
    private List<Integer> mDailyItemReferences = new LinkedList<>();
    private String mContext = "unknown"; // context is some value that can be used to "annotate" information with that value
    // emotion history database
    private Map<Long, Integer> mPastEmotionDiaryEntries = new HashMap<Long, Integer>();
    private int[] mPast7EmotionDiaryEntries = new int[7];
    // The system logger
    protected final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    private int[] mSelectedLowestHighestQuestionnaireOptions = {Integer.MAX_VALUE, Integer.MIN_VALUE}; // [<lowest>, <highest>] ==> [0,3]
    private String[] mSelectedLowestHighestQuestionnaireSubOptions = {"", ""}; // [<lowest>, <highest>] ==> ["a", "b"]

//    private int[] mPreviousSelectedQuestions = {};

    ArrayList mPreviousSelectedQuestions = new ArrayList<>(Arrays.asList(1,3));
    private enum EmotionDiaryDays {
        Mo(0), Di(1), Mi(2), Do(3), Fr(4), Sa(5), So(6);

        private Integer dayNum;

        private EmotionDiaryDays(final Integer num) {
            this.dayNum = num;
        }

        public Integer getNum() {
            return dayNum;
        }

    }

    // sceneflow variables
    private final String mVSM_DiaryDay = "diaryDay";
    private final String mVSM_CurrentDiaryDay = "currentDiaryDay";
    private final String mVSM_DiaryDailyItemsNumber = "diaryDailyItemsNum";
    private final String mVSM_DiaryItemProducer = "diaryItemProducer";
    private final String mVSM_DiaryItemText = "diaryItemText";
    private final String mVSM_DiaryItemTimeCode = "diaryItemTimeCode";
    private final String mVSM_DiaryEmotionDay = "diaryEmotionDay";
    private final String mVSM_DiaryEmotionDayValue = "diaryEmotionDayValue";
    private final String mVSM_DiaryEmotionEntries = "diaryEmotionEntries";

    // File stuff
    private String umDir = "";
    private String umFile = "";

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
            setUserValue("name", activity);
            setUserValue("introduction", activity);
            setUserValue("meditation", activity);
//            setUserValue("break", activity);
//            setUserValue("type", activity);
//            setUserValue("therapy", activity);
//            setUserValue("icd", activity);
//            setUserValue("contact", activity);
//            setUserValue("contactphone", activity);
//            setUserValue("therapist", activity);
//            setUserValue("therapistphone", activity);
//            setUserValue("nextworktime_mo", activity);
//            setUserValue("nextworktime_tu", activity);
//            setUserValue("nextworktime_we", activity);
//            setUserValue("nextworktime_th", activity);
//            setUserValue("nextworktime_fr", activity);
//            setUserValue("actworktime_mo", activity);
//            setUserValue("actworktime_tu", activity);
//            setUserValue("actworktime_we", activity);
//            setUserValue("actworktime_th", activity);
//            setUserValue("actworktime_fr", activity);
//            setUserValue("pos_activitiy", activity);

            // add/update to user profiles
            saveUserModel();
        }

        if (name.equalsIgnoreCase("setcontext")) {
            mLogger.message("Setting context");
            if (activity.get("value") != null) {
                mContext = activity.get("value");
                mLogger.message("Context set to " + mContext);
            }
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
            mLogger.message("Loading Logs!!!!");

            if ((activity.get("name") != null)) {
                String userName = activity.get("name");
                mUser = loadUserData(userName);

                if (mUser != null) {
                    mLogger.message("Data from user " + userName + " found!");
                } else {
                    JSONArray users = mUserProfiles.getJSONArray("users");
                    mUser = createUser(userName, users.length() + 1);
                    users.put(mUser);
                    saveUserModel();
                    mLogger.warning("Data from user " + userName + " not found - new user created!");
                }
            } else { // load the latest
                mUser = loadUserData();
                String userName = mUser.getString("name");
                mLogger.message("Data from user " + userName + " loaded!");
            }
            // at this point, there is a user model, a freshly created or loaded
            diaryDaysManagement();
        }

        // PlayAction("[um currentday]") stores the number of the current day in a sceneflow var (mVSM_CurrentDiaryDay)
        if (name.equalsIgnoreCase("currentday")) {
            if (mProject.hasVariable(mVSM_CurrentDiaryDay)) {
                mProject.setVariable(mVSM_CurrentDiaryDay, mDiaryDays.size());
            }
        }

        // PlayAction("[um diaryday no=0]") stores a day string (e.g., "2. November 2020")
        // in a sceneflow var (mVSM_DiaryDay)
        if (name.equalsIgnoreCase("diaryday")) {
            if ((activity.get("no") != null)) {
                int day = Integer.parseInt(activity.get("no"));

                if (mProject.hasVariable(mVSM_DiaryDay)) {
                    if (mDiaryDays.get(day) != null) {
                        mProject.setVariable(mVSM_DiaryDay, mDiaryDays.get(day).replace(",", " ")); //replace "," with " "
                    } else {
                        mLogger.failure("Requested diary day (" + day + ") does not exist");
                    }
                }
            }
        }

        // PlayAction ( "[um dailyitems day=0]" ) stores the number of dialog parts
        // in a sceneflow var (mVSM_DiaryDailyItemsNumber)
        if (name.equalsIgnoreCase("dailyitems")) {
            if ((activity.get("day") != null)) {
                mSelectedDay = Integer.parseInt(activity.get("day"));

                // filter by context, if present
                String cont = null;
                if ((activity.get("context") != null)) {
                    cont = activity.get("context");
                }

                if (mProject.hasVariable(mVSM_DiaryDailyItemsNumber)) {
                    if ((mSelectedDay < mDiaryDays.size()) && (mDiaryDays.get(mSelectedDay) != null)) {
                        String dayStr = mDiaryDays.get(mSelectedDay);

                        if (cont != null) {
                            mDailyItemReferences = collectDailyItems(dayStr, cont);
                        } else {
                            mDailyItemReferences = collectDailyItems(dayStr);
                        }
                        mProject.setVariable(mVSM_DiaryDailyItemsNumber, mDailyItemReferences.size());
                    } else {
                        mLogger.failure("Requested diary day (" + mSelectedDay + ") does not exist");
                    }
                }
            } else {
                mLogger.failure("Required 'day' information (int) is missing for loading daily items.");
            }
        }

        //PlayAction ( "[um item no=" + dcnt + "]" ) saves producer and text of a diary item (no) in sceneflow vars
        //(mVSM_DiaryItemProducer, mVSM_DiaryItemText) containing the informatio about the dialog item
        if (name.equalsIgnoreCase("item")) {
            if (activity.get("no") != null) {
                int no = Integer.parseInt(activity.get("no"));

                if (mProject.hasVariable(mVSM_DiaryItemProducer) && mProject.hasVariable(mVSM_DiaryItemText) && mProject.hasVariable(mVSM_DiaryItemTimeCode)) {
                    if ((mSelectedDay < mDiaryDays.size()) && (mDiaryDays.get(mSelectedDay) != null)) {
                        String dayStr = mDiaryDays.get(mSelectedDay);
                        int itemNum = mDailyItemReferences.get(no);
                        mLogger.message("Retrieving dialog part represented with no " + itemNum);

                        JSONArray diary = mUser.getJSONArray("diary");
                        JSONObject entry;

                        String context = (activity.get("context") != null) ? activity.get("context") : null;

                        for (int i = 0; i < diary.length(); i++) {
                            if (diary.getJSONObject(i).getInt("no") == itemNum) {
                                entry = diary.getJSONObject(i);
                                if ((entry.has("entry")) && (!entry.getString("entry").isEmpty())) { // for now, do only consider entries with key "entry"
                                    // if context information is present, filter further by context
                                    if (context != null) { // if cmd comes with context filter, filter item accordingly
                                        if ((entry.has("context"))) {
                                            if (entry.getString("context").equalsIgnoreCase(context)) {
                                                mProject.setVariable(mVSM_DiaryItemProducer, new StringValue(entry.getString("producer")));
                                                mProject.setVariable(mVSM_DiaryItemText, new StringValue(entry.getString("entry")));
                                            }
                                        }
                                    } else {
                                        mProject.setVariable(mVSM_DiaryItemProducer, new StringValue(entry.getString("producer")));
                                        mProject.setVariable(mVSM_DiaryItemText, new StringValue(entry.getString("entry")));
                                    }
                                    mProject.setVariable(mVSM_DiaryItemTimeCode, new StringValue(String.valueOf(entry.getLong("date"))));
                                }
                                return;
                            }
                        }
                        mProject.setVariable(mVSM_DiaryItemProducer, new StringValue("unknown"));
                        mProject.setVariable(mVSM_DiaryItemText, new StringValue("No entry referred by " + mSelectedDay));
                        mLogger.failure("Requested diary day (" + mSelectedDay + ") does not exist");
                    }
                }
            } else {
                mLogger.failure("Required 'no' information (int) is missing for loading item.");
            }
        }

        if (name.equalsIgnoreCase("updateitem")) {
            long dateid = Long.parseLong(activity.get("dateid"));
            String value = activity.get("value");
            // strip "'";
            if (value != null) {
                value = value.replace("'", "");
            } else {
                value = "";
            }

            JSONArray diary = mUser.getJSONArray("diary");

            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);
                long dateMillis = diaryItem.getLong("date");
                if (diaryItem.getLong("date") == dateid) { // id dateid matches replace entry with new text
                    diaryItem.put("entry", value);
                    diary.put(i, diaryItem);
                    saveUserModel();
                    break; // exit loop since there (should) be only one item
                }
            }
        }

        if (name.equalsIgnoreCase("collect_diaryemotions")) {
            collectEmotionEntries();
            // today - normalize
            Calendar cal = Calendar.getInstance();
            cal.set(Calendar.HOUR_OF_DAY, 0);
            cal.set(Calendar.MINUTE, 0);
            cal.set(Calendar.SECOND, 0);
            cal.set(Calendar.MILLISECOND, 0);
            Date workDate = cal.getTime();
            // go through methodically
            SortedSet<Long> keys = new TreeSet<>(mPastEmotionDiaryEntries.keySet());
            for (int dayCnt = 0; dayCnt < 7; dayCnt++) { // goes up to 6 (represents current day and six days in the past)
                // initialize number and occurances
                int emoValue = 0;
                int emoValueCnt = 0;
                // sort Dates
                for (Long key : keys) {
                    int value = mPastEmotionDiaryEntries.get(key);
                    // normalize time
                    Date entryDate = new Date(key);
                    cal.setTime(entryDate);
                    cal.set(Calendar.HOUR_OF_DAY, 0);
                    cal.set(Calendar.MINUTE, 0);
                    cal.set(Calendar.SECOND, 0);
                    cal.set(Calendar.MILLISECOND, 0);
                    Date normEntryDate = cal.getTime();
                    // do if there is something
                    if (workDate.getTime() == normEntryDate.getTime()) {
                        // if more than one value found, calculate the mean
                        emoValueCnt++;
                        emoValue = emoValue + value;
                    }
                }
                // Normalize emotion value
                emoValue = (int) Math.ceil((double) emoValue / emoValueCnt);
                // store the value in days array, 0 is today
                mPast7EmotionDiaryEntries[dayCnt] = emoValue;
                // update work day
                cal.setTime(workDate);
                cal.add(Calendar.DATE, -1); // go back 7 days, increment 1, start with 0 = current day
                workDate = cal.getTime();
            }
            mLogger.message("Found emotion diary entries " + Arrays.toString(mPast7EmotionDiaryEntries));

            int emoEntries = 0;
            for (int dailyEmotion : mPast7EmotionDiaryEntries) {
                emoEntries = (dailyEmotion > 0) ? emoEntries + 1 : emoEntries;
            }

            mLogger.message("Found emotion diary entries " + emoEntries);

            mProject.setVariable(mVSM_DiaryEmotionEntries, emoEntries);
        }

        if (name.equalsIgnoreCase("get_diaryemotion")) {
            if ((activity.get("no") != null) || (activity.get("today") != null)) {
                int no = Integer.parseInt(activity.get("no"));
                String todayStr = activity.get("today");
                EmotionDiaryDays todayDiaryDayNum = EmotionDiaryDays.valueOf(todayStr);
                int weeklyDayNum = todayDiaryDayNum.getNum();
                mLogger.message("Todays diary day offset is " + weeklyDayNum);
                // get the day but mod 7
                int queriedDay = (weeklyDayNum - no >= 0) ? weeklyDayNum - no : 7 + no;
                EmotionDiaryDays day = todayDiaryDayNum.values()[queriedDay % 7];
                // update model variables
                mProject.setVariable(mVSM_DiaryEmotionDay, day.name());
                mProject.setVariable(mVSM_DiaryEmotionDayValue, mPast7EmotionDiaryEntries[no]);

            } else {
                mLogger.failure("Required 'no' (int), or 'day' (String, e.g., Mo, Di, ...,  So) information is missing for loading item.");
            }
        }

        if (name.equalsIgnoreCase("diary_emotion")) {
            if (mUser != null) {
                storeDiaryEmotionEntry(activity, "value");
            } else {
                mLogger.warning("No user specified, diary emotion value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_mood")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "User", "mood", "value");
            } else {
                mLogger.warning("No user specified, diary mood value will not be stored.");
            }
        }

        // final emotional user model entries (LIWC)
        if (name.equalsIgnoreCase("diary_posaffect")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "posaffect", "value");
            } else {
                mLogger.warning("No user specified, diary posaffect value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_negaffect")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "negaffect", "value");
            } else {
                mLogger.warning("No user specified, diary negaffect value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_optimism")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "optimism", "value");
            } else {
                mLogger.warning("No user specified, diary optimism value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_anxiety")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "anxiety", "value");
            } else {
                mLogger.warning("No user specified, diary anxiety value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_discrepancy")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "discrepancy", "value");
            } else {
                mLogger.warning("No user specified, diary discrepancy value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_insight")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "insight", "value");
            } else {
                mLogger.warning("No user specified, diary insight value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_tentative")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "tentative", "value");
            } else {
                mLogger.warning("No user specified, diary tentative value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_pronoun")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "pronoun", "value");
            } else {
                mLogger.warning("No user specified, diary pronoun value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_myself")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "myself", "value");
            } else {
                mLogger.warning("No user specified, diary myself value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_we")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "we", "value");
            } else {
                mLogger.warning("No user specified, diary we value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_self")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "self", "value");
            } else {
                mLogger.warning("No user specified, diary self value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_you")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "you", "value");
            } else {
                mLogger.warning("No user specified, diary you value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_other")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "diary_other", "value");
            } else {
                mLogger.warning("No user specified, diary diary_other value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_body")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "body", "value");
            } else {
                mLogger.warning("No user specified, diary body value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_sex")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "sex", "value");
            } else {
                mLogger.warning("No user specified, diary sex value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_eat")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "eat", "value");
            } else {
                mLogger.warning("No user specified, diary eat value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_sleep")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "sleep", "value");
            } else {
                mLogger.warning("No user specified, diary sleep value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_school")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "school", "value");
            } else {
                mLogger.warning("No user specified, diary school value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_job")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "job", "value");
            } else {
                mLogger.warning("No user specified, diary job value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary_achieve")) {
            if (mUser != null) {
                storeUserDiaryEntry(activity, "achieve", "value");
            } else {
                mLogger.warning("No user specified, diary achieve value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("avatar_animation")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "producer", "avatar_animation", "value");
            } else {
                mLogger.warning("No user specified, diary avatar_animation value will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("diary")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "producer", "entry", "entry");
            } else {
                mLogger.warning("No user specified, diary entry will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("utterancestart")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "producer", "utterancestart", "value");
            } else {
                mLogger.warning("No user specified, diary entry will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("utteranceduration")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "producer", "utteranceduration", "value");
            } else {
                mLogger.warning("No user specified, diary entry will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("log")) {
            if (mUser != null) {
                storeDiaryEntry(activity, "producer", "log", "value");
            } else {
                mLogger.warning("No user specified, diary log entry will not be stored.");
            }
        }

        if (name.equalsIgnoreCase("calculateStats")) {
            this.calculateStats(activity.get("context"));
        }

        if (name.equalsIgnoreCase("randGenQuestionNum")) {
            mLogger.message("Generating random questionnaire question number");
            this.identifyHighestLowestQuestionnaireOption(activity.get("context"));
            this.randGenQuestionNum(activity.get("context"), activity.get("level"));
            if (mPreviousSelectedQuestions.size() == 2){
                this.resetHighestLowestQuestionnaireOptions();
            }
        }

        if (name.equalsIgnoreCase("loadSelectedQuestionnaireOption")) {
            mLogger.message("Loading questionnaire selected options for question number " + activity.get("frage_n"));
            this.loadSelectedQuestionnaireOptionForQ(activity.get("context"), activity.get("frage_n"));
        }

        if (name.equalsIgnoreCase("resetHighestLowestQuestionnaireOptions")){
            mLogger.message("Resetting internal variables for random question selection.");
            this.resetHighestLowestQuestionnaireOptions();
        }
    }

    private void calculateStats(String context) {
        JSONArray diary = mUser.getJSONArray("diary");
        if (diary.length() > 0 && !context.equals("")) {
            float mean = calculateMean(diary, context);
            this.mProject.setVariable("mean", mean);
            this.mProject.setVariable("sd", calculateSD(diary, context, mean));
        }
    }


    // set also overrides previously set values.
    private void setUserValue(String key, AbstractActivity activity) {
        if ((activity.get(key) != null) && (mUser.get(key) != "")) {
            String value = activity.get(key).replace("'", "");
            mLogger.failure("Setting " + key + " with activity key " + value);
            mUser.remove(key);
            mUser.put(key, value);
        }
    }

    // get String value.
    private String getUserStrValue(AbstractActivity activity) {
        final String key = "value";
        if ((activity.get(key) != null) && (mUser.has(activity.get(key))) && (mUser.get(activity.get(key)) != "")) {
            return mUser.getString(activity.get(key));
        } else {
            mLogger.failure("Failed to retrieve user model information " + activity.get(key) + " with activity key " + key);
            mLogger.failure(mUser.toString());
            return "";
        }
    }

    // get Integer value.
    private Integer getUserIntValue(AbstractActivity activity) {
        final String key = "value";
        if ((activity.get(key) != null) && (mUser.has(activity.get(key))) && (mUser.get(activity.get(key)) != "")) {
            return Integer.parseInt(mUser.getString(activity.get(key)));
        } else {
            mLogger.failure("Failed to retrieve user model information " + activity.get(key) + " with activity key " + key);
            mLogger.failure(mUser.toString());
            return -1;
        }
    }

    private synchronized void storeUserDiaryEntry(AbstractActivity activity, String key, String value) {
        storeDiaryEntry(activity, "User", key, value);
    }

    private synchronized void storeDiaryEntry(AbstractActivity activity, String producer, String key, String value) {
        JSONObject diaryentry = new JSONObject();

        long dateMillis = System.currentTimeMillis();

        diaryentry.put("date", dateMillis);
        diaryentry.put("no", getLastDiaryEntryNumber() + 1);
        diaryentry.put("producer", (activity.get(producer) != null) ? activity.get(producer) : producer);
        diaryentry.put(key, (activity.get(value) != null) ? activity.get(value).replace("'", "").replace("\n", " ").replace("  ", " ") : "");
        diaryentry.put("context", mContext);

        JSONArray diary = mUser.getJSONArray("diary");
        diary.put(diaryentry);

        saveUserModel();
        diaryDaysManagement();
    }

    private synchronized void storeDiaryEmotionEntry(AbstractActivity activity, String emotionValue) {
        JSONObject diaryentry = new JSONObject();

        long dateMillis = System.currentTimeMillis();

        diaryentry.put("date", dateMillis);
        diaryentry.put("no", getLastDiaryEntryNumber() + 1);
        diaryentry.put("producer", "User");
        diaryentry.put("context", mContext);

        String emotion = (activity.get(emotionValue) != null) ? activity.get(emotionValue).replace("'", "").replace("\n", " ").replace("  ", " ") : "";

        int value = -1;
        switch (emotion) {
            case "em_very_sad": {
                value = 1;
                break;
            }
            case "em_sad": {
                value = 2;
                break;
            }
            case "em_neutral": {
                value = 3;
                break;
            }
            case "em_happy": {
                value = 4;
                break;
            }
            case "em_very_happy": {
                value = 5;
                break;
            }
        }

        diaryentry.put("emotion", value);

        JSONArray diary = mUser.getJSONArray("diary");
        diary.put(diaryentry);

        saveUserModel();
        diaryDaysManagement();
    }

    private void collectEmotionEntries() {
        JSONArray diary = mUser.getJSONArray("diary");

        if (diary.length() > 0) {
            mLogger.message("Found " + diary.length() + " diary entries.");

            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);

                // make a proper date
                long dateMillis = diaryItem.getLong("date");

                // not every entry is an emotion entry; collect only those.
                if (diaryItem.has("emotion")) {
                    mPastEmotionDiaryEntries.put(dateMillis, diaryItem.getInt("emotion"));
                }
            }
        }
    }

    private void loadSelectedQuestionnaireOptionForQ(String context, String frage_n) {
        JSONArray diary = mUser.getJSONArray("diary");

        mLogger.message("Identifying lowest and highest selected option from: " + context + "for frage_n= " + frage_n);

        for (int i = 0; i < diary.length(); i++) {
            JSONObject diaryItem = diary.getJSONObject(i);

            // not every entry is for the context of Ubidenz Questionnaire; collect only those.
            if (diaryItem.has("context")) {
                if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {


                    String q_n_diary = ((String) diaryItem.get("producer"));

                    if (q_n_diary.equals(frage_n)) {
                        String antwort_option = ((String) diaryItem.get("log"));
                        mLogger.message("For Frage number " + frage_n + " the selected option was: " + antwort_option);
                        this.mProject.setVariable("antwort_option", antwort_option);
                        break;
                    }
                }
            }
        }
    }

    private void resetHighestLowestQuestionnaireOptions() {
        mSelectedLowestHighestQuestionnaireOptions = new int[]{Integer.MAX_VALUE, Integer.MIN_VALUE}; // [<lowest>, <highest>] ==> [0,3]
        mSelectedLowestHighestQuestionnaireSubOptions = new String[]{"", ""}; // [<lowest>, <highest>] ==> ["a", "b"]
        mPreviousSelectedQuestions = new ArrayList<>(Arrays.asList(1,3));
    }

    private void identifyHighestLowestQuestionnaireOption(String context) {
        JSONArray diary = mUser.getJSONArray("diary");

        mLogger.message("Identifying lowest and highest selected option from: " + context);
        List<String> levels = new ArrayList<String>() {{
            add("lowest");
            add("highest");
        }};
        for (String level : levels) {
            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);

                // not every entry is for the context of Ubidenz Questionnaire; collect only those.
                if (diaryItem.has("context")) {
                    if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {
                        // Splitting the string into number and alphabet parts
                        int optionNumberPart = Integer.valueOf(((String) diaryItem.get("log")).replaceAll("[^0-9]", ""));
                        String optionAlphabetPart = ((String) diaryItem.get("log")).replaceAll("[^A-Za-z]", "");

                        if (level.equals("highest")) {
                            if (optionNumberPart < mSelectedLowestHighestQuestionnaireOptions[0]) {
                                mSelectedLowestHighestQuestionnaireOptions[0] = optionNumberPart;
                                mSelectedLowestHighestQuestionnaireSubOptions[0] = optionAlphabetPart;
                            }
                        } else {
                            if (optionNumberPart > mSelectedLowestHighestQuestionnaireOptions[1]) {
                                mSelectedLowestHighestQuestionnaireOptions[1] = optionNumberPart;
                                mSelectedLowestHighestQuestionnaireSubOptions[1] = optionAlphabetPart;
                            }
                        }
                    }
                }
            }
        }
        mLogger.message("Lowest and highest option in " + context + " are: " + Arrays.toString(mSelectedLowestHighestQuestionnaireOptions));
    }

    private void randGenQuestionNum(String context, String level) {

        JSONArray diary = mUser.getJSONArray("diary");

        mLogger.message("Selecting random question from " + level + " selected options in the context of " + context);
        int answer_val;
        List<Integer> possibleQuestionSetHighest = new ArrayList<Integer>();
        List<Integer> possibleQuestionSetLowest = new ArrayList<Integer>();
        for (int i = 0; i < diary.length(); i++) {
            JSONObject diaryItem = diary.getJSONObject(i);

            // not every entry is for the context of Ubidenz Questionnaire; collect only those.
            if (diaryItem.has("context")) {
                if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {


                    if (level.equals("highest")) {
                        // Splitting the string into number and alphabet parts
                        answer_val = Integer.valueOf(((String) diaryItem.get("log")).replaceAll("[^0-9]", ""));
                        if (answer_val == mSelectedLowestHighestQuestionnaireOptions[1]) {
                            possibleQuestionSetHighest.add(Integer.valueOf((String) diaryItem.get("producer")));
                        }
                    } else {
                        // Splitting the string into number and alphabet parts
                        answer_val = Integer.valueOf(((String) diaryItem.get("log")).replaceAll("[^0-9]", ""));
                        if (answer_val == mSelectedLowestHighestQuestionnaireOptions[0]) {
                            possibleQuestionSetLowest.add(Integer.valueOf((String) diaryItem.get("producer")));
                        }
                    }
                }
            }
        }

//            TODO: Send one question number and associated selected option number. ==> Currently sending only one question number randomly.
//            Best solution: Collect all questions and associated answers in a dict
//            Go through the dict and find the highest value and lowest value
//            Form a sub dict with all entries with highest/lowest value
//            Return one question and associated answer with highest value and one for lowest value

        Random rand = new Random();
        if (level.equals("highest")) {

            int randomHighestOptionQ;
            do {
                int randomHighestOptionQPos = rand.nextInt(possibleQuestionSetHighest.size());
                randomHighestOptionQ = possibleQuestionSetHighest.get(randomHighestOptionQPos);
            } while (mPreviousSelectedQuestions.contains(randomHighestOptionQ));
//            int randomHighestOptionQPos = rand.nextInt(possibleQuestionSetHighest.size());
//            int randomHighestOptionQ = possibleQuestionSetHighest.get(randomHighestOptionQPos);
            mPreviousSelectedQuestions.add(randomHighestOptionQ);

            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);

                // not every entry is for the context of Ubidenz Questionnaire; collect only those.
                if (diaryItem.has("context")) {
                    if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {

                        if (diaryItem.get("producer").equals(Integer.toString(randomHighestOptionQ))) {
                            String optionAlphabetPart = ((String) diaryItem.get("log")).replaceAll("[^A-Za-z]", "");
                            mSelectedLowestHighestQuestionnaireSubOptions[1] = optionAlphabetPart;
                        }
                    }
                }
            }
            String antwort_option = Integer.toString(mSelectedLowestHighestQuestionnaireOptions[1]) + mSelectedLowestHighestQuestionnaireSubOptions[1];

            this.mProject.setVariable("frage_n", randomHighestOptionQ);
            this.mProject.setVariable("antwort_option", antwort_option);
        } else {
            int randomLowestOptionQ;
            do {
                int randomLowestOptionQPos = rand.nextInt(possibleQuestionSetLowest.size());
                randomLowestOptionQ = possibleQuestionSetLowest.get(randomLowestOptionQPos);
            } while (mPreviousSelectedQuestions.contains(randomLowestOptionQ));

//            int randomLowestOptionQPos = rand.nextInt(possibleQuestionSetLowest.size());
//            int randomLowestOptionQ = possibleQuestionSetLowest.get(randomLowestOptionQPos);
            mPreviousSelectedQuestions.add(randomLowestOptionQ);


            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);

                // not every entry is for the context of Ubidenz Questionnaire; collect only those.
                if (diaryItem.has("context")) {
                    if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {

                        if (diaryItem.get("producer").equals(Integer.toString(randomLowestOptionQ))) {
                            String optionAlphabetPart = ((String) diaryItem.get("log")).replaceAll("[^A-Za-z]", "");
                            mSelectedLowestHighestQuestionnaireSubOptions[0] = optionAlphabetPart;
                        }
                    }
                }
            }
            String antwort_option = Integer.toString(mSelectedLowestHighestQuestionnaireOptions[0]) + mSelectedLowestHighestQuestionnaireSubOptions[0];

            this.mProject.setVariable("frage_n", randomLowestOptionQ);
            this.mProject.setVariable("antwort_option", antwort_option);
        }

    }

    private Float calculateMean(JSONArray diary, String context) {
        mLogger.message("Calculating mean on " + context);
        int count = 0;
        int sum = 0;
        for (int i = 0; i < diary.length(); i++) {
            JSONObject diaryItem = diary.getJSONObject(i);

            // not every entry is for the context of Ubidenz Questionnaire; collect only those.
            if (diaryItem.has("context")) {
                if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {
                    sum += Integer.valueOf((String) diaryItem.get("log"));
                    count++;
                }
            }
        }
        return (float) sum / count;

    }

    private Float calculateSD(JSONArray diary, String context, float mean) {

        mLogger.message("Calculating mean on " + context);

        int count = 0;
        double sum = 0;
        for (int i = 0; i < diary.length(); i++) {
            JSONObject diaryItem = diary.getJSONObject(i);

            // not every entry is for the context of Ubidenz Questionnaire; collect only those.
            if (diaryItem.has("context")) {
                if (diaryItem.get("context").equals(context) && diaryItem.has("log")) {
                    sum = sum + Math.pow(Double.valueOf((String) diaryItem.get("log")) - mean, 2.0);
                    count++;
                }
            }
        }
        return (float) Math.sqrt(sum / count);

    }

    private void diaryDaysManagement() {
        // do diary management
        collectDiaryDays();

        if (mProject.hasVariable("diaryDaysNum")) {
            mProject.setVariable("diaryDaysNum", mDiaryDays.size());
        }
    }

    private void collectDiaryDays() {
        mDiaryDays = new LinkedList<>();
        Map<Date, JSONArray> dateDiaryEntries = new HashMap<>();
        JSONArray diary = mUser.getJSONArray("diary");

        if (diary.length() > 0) {
            mLogger.message("Found " + diary.length() + " diary entries.");

            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);

                // make a proper date
                long dateMillis = diaryItem.getLong("date");
                Date exactDate = new Date(dateMillis);
                // date format
                DateFormat df = new SimpleDateFormat("MMMM d, yyyy", Locale.GERMANY);
                // reduce it to month day year to "collect" all item for one day!
                String dateStr = df.format(exactDate);
                Date reducedDate = null;
                try {
                    reducedDate = df.parse(dateStr);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
                // sort it
                if (dateDiaryEntries.containsKey(reducedDate)) { // if there is an entry for this particular date, add diary entry
                    JSONArray dateEntries = dateDiaryEntries.get(reducedDate);
                    dateEntries.put(diaryItem);
                    dateDiaryEntries.replace(reducedDate, dateEntries);
                } else { // if there is not an entry for this particular date, create diary entry array and add diary entrey
                    JSONArray dateEntries = new JSONArray();
                    dateEntries.put(diaryItem);
                    dateDiaryEntries.put(reducedDate, dateEntries);
                }
            }
            for (Map.Entry<Date, JSONArray> entry : dateDiaryEntries.entrySet()) {
                Date k = entry.getKey();
                DateFormat df = new SimpleDateFormat("MMMM d, yyyy", Locale.GERMANY);
                String dateStr = df.format(k);
                mDiaryDays.add(dateStr);
            }
        }
        mLogger.message("Found entries for " + dateDiaryEntries.keySet().size() + " different days.");
    }

    private List<Integer> collectDailyItems(String dayStr) {
        JSONArray diary = mUser.getJSONArray("diary");
        List<Integer> entries = new LinkedList<>();

        // Date format
        DateFormat df = new SimpleDateFormat("MMMM d, yyyy", Locale.GERMANY);
        Date targetDate = Calendar.getInstance().getTime(); // initialize it with today

        try { // try to parse dayStr and create Date object
            targetDate = df.parse(dayStr);
        } catch (ParseException e) {
            mLogger.failure(dayStr + " is not a valid day (date)!");
        }
        if (diary.length() > 0) {
            mLogger.message("Found " + diary.length() + " diary entries.");
            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);
                if ((diaryItem.has("entry")) && (!diaryItem.getString("entry").isEmpty())) { // for now, do only consider entries with key "entry"
                    // make a proper date
                    long dateMillis = diaryItem.getLong("date");
                    // get the number of the dialog entry
                    Date exactDate = new Date(dateMillis);
                    // reduce it to month day year to "collect" all item for one day!
                    String dateStr = df.format(exactDate);
                    Date reducedDate = null;
                    try {
                        reducedDate = df.parse(dateStr);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                    if (reducedDate.equals(targetDate)) {
                        entries.add(diaryItem.getInt("no"));
                    }
                }
            }
        }
        return entries;
    }

    private List<Integer> collectDailyItems(String dayStr, String context) {
        JSONArray diary = mUser.getJSONArray("diary");
        List<Integer> entries = new LinkedList<>();

        // Date format
        DateFormat df = new SimpleDateFormat("MMMM d, yyyy", Locale.GERMANY);
        Date targetDate = Calendar.getInstance().getTime(); // initialize it with today

        try { // try to parse dayStr and create Date object
            targetDate = df.parse(dayStr);
        } catch (ParseException e) {
            mLogger.failure(dayStr + " is not a valid day (date)!");
        }
        if (diary.length() > 0) {
            mLogger.message("Found " + diary.length() + " diary entries. Filtering for context " + context);
            for (int i = 0; i < diary.length(); i++) {
                JSONObject diaryItem = diary.getJSONObject(i);
                if ((diaryItem.has("entry"))
                        && (!diaryItem.getString("entry").isEmpty())
                        && (!diaryItem.getString("context").isEmpty())
                        && (diaryItem.getString("context").equalsIgnoreCase(context))) { // for now, do only consider entries with key "entry"
                    // make a proper date
                    long dateMillis = diaryItem.getLong("date");
                    // get the number of the dialog entry
                    Date exactDate = new Date(dateMillis);
                    // reduce it to month day year to "collect" all item for one day!
                    String dateStr = df.format(exactDate);
                    Date reducedDate = null;
                    try {
                        reducedDate = df.parse(dateStr);
                    } catch (ParseException e) {
                        e.printStackTrace();
                    }
                    if (reducedDate.equals(targetDate)) {
                        entries.add(diaryItem.getInt("no"));
                    }
                }
            }
        }
        return entries;
    }

    private long getLastDiaryEntryNumber() {
        JSONArray diary = mUser.getJSONArray("diary");
        long biggestNo = -1;

        for (int i = 0; i < diary.length(); i++) {
            JSONObject item = diary.getJSONObject(i);
            biggestNo = (item.getLong("no") > biggestNo) ? item.getLong("no") : biggestNo;
        }
        return biggestNo;
    }

    private JSONObject loadUserData(String name) {
        JSONArray users = mUserProfiles.getJSONArray("users");
        for (int i = 0; i < users.length(); i++) {
            JSONObject item = users.getJSONObject(i);
            if (item.getString("name").equalsIgnoreCase(name)) return item;
        }
        return null;
    }

    private JSONObject loadUserData() { // load data from device user
        JSONArray users = mUserProfiles.getJSONArray("users");

        if (users.length() > 0) {
            return users.getJSONObject(0);
        } else {
            loadUserModel();
            return users.getJSONObject(0);
        }
    }

    private JSONObject createUser(String name, long id) {
        JSONObject user = new JSONObject();

        // initial user data
        user.put("name", name);
        user.put("id", id);
        user.put("introduction", "unknown");
        user.put("diarymenu", "unknown");
        user.put("meditation", "unknown");
//        user.put("break", "unknown");
//        user.put("type", "unknown");
//        user.put("therapy", "unknown");
//        user.put("icd", "unknown");
//        user.put("contact", "unknown");
//        user.put("contactphone", "unknown");
//        user.put("therapist", "unknown");
//        user.put("therapistphone", "unknown");
//        user.put("nextworktime_mo", "unknown");
//        user.put("nextworktime_tu", "unknown");
//        user.put("nextworktime_we", "unknown");
//        user.put("nextworktime_th", "unknown");
//        user.put("nextworktime_fr", "unknown");
//        user.put("actworktime_mo", "unknown");
//        user.put("actworktime_tu", "unknown");
//        user.put("actworktime_we", "unknown");
//        user.put("actworktime_th", "unknown");
//        user.put("actworktime_fr", "unknown");
//        user.put("posactivity", "unknown");

        // intial diary entry
        JSONArray diary = new JSONArray();
        JSONObject diaryentry = new JSONObject();

        String dateStr = Long.toString(System.currentTimeMillis());

        diaryentry.put("date", dateStr);
        diaryentry.put("no", 0);
        diaryentry.put("producer", "system");
        diaryentry.put("entry", "created");
        diaryentry.put("context", mContext);

        //diary.put(diaryentry);
        user.put("diary", diary);

        return user;
    }

    private void loadUserModel() {
        mLogger.message("Loading EmmA User Model ...");

        if ((mConfig.getProperty("umdir") != null) && (!mConfig.getProperty("umdir").isEmpty())) {
            umDir = mConfig.getProperty("umdir");
        } else {
            mLogger.failure("<Feature key=\"umdir\" val=\"<directory>\"/> is not specified in VSM project file. Aborting!");
            System.exit(0);
        }
        if ((mConfig.getProperty("umfile") != null) && (!mConfig.getProperty("umfile").isEmpty())) {
            umFile = mConfig.getProperty("umfile");
        } else {
            mLogger.failure("<Feature key=\"umfile\" val=\"<file name>\"/> is not specified in VSM project file. Aborting!");
            System.exit(0);
        }

        String umf = (mProject.getProjectPath() + File.separator + umDir + File.separator + umFile).replace("\\", "/");
        String input = "";
        try {
            BufferedReader br = new BufferedReader(new FileReader(umf));
            StringBuilder sb = new StringBuilder();
            String line = br.readLine();
            while (line != null) {
                sb.append(line);
                line = br.readLine();
                //mLogger.message("Raw input line: " + line);
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

        //mLogger.message("Raw input: " + input);

        mUserProfiles = new JSONObject(input);

        saveUserModel();

        JSONArray users = mUserProfiles.getJSONArray("users");

        mLogger.message("Found user " + users.get(0).toString());
    }

    private synchronized void saveUserModel() {
        String umf = (mProject.getProjectPath() + File.separator + umDir + File.separator + umFile).replace("\\", "/");
        synchronized (mUserProfiles) {
            try {
                FileWriter umfw = new FileWriter(umf);

                umfw.write(mUserProfiles.toString());
                umfw.flush();
                umfw.close();
            } catch (IOException e) {
                mLogger.failure("Error writing UM to " + umf);
            }
        }
    }

    @Override
    public void unload() {
        // save user data in user Model
        saveUserModel();
    }
}
