package de.dfki.vsm.players.stickman;

import de.dfki.vsm.editor.event.SceneExecutedEvent;
import de.dfki.vsm.editor.event.TurnExecutedEvent;
import de.dfki.vsm.editor.event.UtteranceExecutedEvent;
import de.dfki.vsm.model.project.PlayerConfig;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.model.scenescript.AbstractWord;
import de.dfki.vsm.model.scenescript.ActionObject;
import de.dfki.vsm.model.scenescript.SceneAbbrev;
import de.dfki.vsm.model.scenescript.SceneGroup;
import de.dfki.vsm.model.scenescript.SceneObject;
import de.dfki.vsm.model.scenescript.SceneParam;
import de.dfki.vsm.model.scenescript.SceneScript;
import de.dfki.vsm.model.scenescript.SceneTurn;
import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.model.scenescript.SceneWord;
import de.dfki.vsm.players.EventActionPlayer;
import de.dfki.vsm.players.action.Action;
import de.dfki.vsm.players.action.ActionListener;
import de.dfki.vsm.players.action.sequence.TimeMark;
import de.dfki.vsm.players.action.sequence.Word;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.action.StickmanAction;
import de.dfki.vsm.players.stickman.action.StickmanEventAction;
import de.dfki.vsm.runtime.interpreter.Process;
import de.dfki.vsm.runtime.players.RunTimePlayer;
import de.dfki.vsm.runtime.values.AbstractValue;
import de.dfki.vsm.runtime.values.AbstractValue.Type;
import de.dfki.vsm.runtime.values.StringValue;
import de.dfki.vsm.runtime.values.StructValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Semaphore;

/**
 *
 * @author Patrick Gebhard (based on Default ScenePlayer)
 *
 */
public final class StickmanScenePlayer implements RunTimePlayer, ActionListener {

	// The singelton player instance
	public static StickmanScenePlayer sInstance = null;
	// The singelton logger instance
	private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();
	// The player's runtime project 
	private RunTimeProject mProject;
	// The project specific config
	private PlayerConfig mPlayerConfig;
	// The project specific name
	private String mPlayerName;
	// The ScheduledActionPlayer
	//private ScheduledActionPlayer mActionPlayer;
	private EventActionPlayer mActionPlayer;
	// Synchronisation with the action player
	private Semaphore mAllActionSync;
	private Semaphore mAllEndSync;
	//private final SynchronousQueue<String> mAllActionsPlayed = new SynchronousQueue<>();
	// The StickmanStage
	private final static StickmanStage mStickmanStage = StickmanStage.getInstance();
	// time mark stuff
	private long mTimeMarkCnt = 0;

	// Construct the StickmanScene player
	private StickmanScenePlayer() {
	}

	// Get the StickmanScenePlayer instance
	public static synchronized StickmanScenePlayer getInstance() {
		if (sInstance == null) {
			sInstance = new StickmanScenePlayer();
		}
		return sInstance;
	}

	@Override
	public void update(Action a, ActionListener.STATE actionState) {
		if (actionState == ActionListener.STATE.ACTION_STARTED) {
			//mLogger.message(((StickmanAction) a).mName + " started");
		}

		if (actionState == ActionListener.STATE.ACTION_FINISHED) {
			//mLogger.message(((StickmanAction) a).mName + " finished");
		}
	}

	@Override
	public void update(ActionListener.STATE actionState) {
		if (actionState == ActionListener.STATE.ALL_ACTIONS_FINISHED) {
			mAllActionSync.release();
		}
	}

	// Launch the StickmanScenePlayer
	@Override
	public final boolean launch(final RunTimeProject project) {
		// Initialize the project
		mProject = project;
		// Initialize the name
		mPlayerName = project.getPlayerName(this);
		// Initialize the config
		mPlayerConfig = project.getPlayerConfig(mPlayerName);
		// Print some information
		mLogger.message("Launching the StickmanScenePlayer '" + this + "' with configuration:\n" + mPlayerConfig);
		// launch ActionPlayer
		mActionPlayer = EventActionPlayer.getInstance();
		// tell the ActionPlayer that StickmanScenePlayer is interested in upates
		mActionPlayer.addListener(this);
		mActionPlayer.start();
		// build the Stage
		getCharacters(mProject.getSceneScript()).stream().forEach((c) -> {
			StickmanStage.addStickman(c);
		});
		// configure blocking mechanism 
		mAllActionSync = new Semaphore(0);
		// configure clean shutdown mechanism
		mAllEndSync = new Semaphore(0);
		return true;
	}

	// Unload the StickmanScenePlayer
	@Override
	public final boolean unload() {
		// Print some information
		mLogger.message("Unloading the StickmanScenePlayer '" + this + "' with configuration:\n" + mPlayerConfig);
		// remove action player
		mActionPlayer.removeListener(this);
		// stop the ActionPlayer
		mActionPlayer.end();
		// do not wait for the end of any actions
		mAllActionSync.release();
		// clear the stage
		StickmanStage.clearStage();
		// Return true at success
		return true;
	}

	private Set<String> getCharacters(SceneScript scenescript) {
		Set<String> speakersSet = new HashSet<>();

		for (SceneObject scene : scenescript.getSceneList()) {
			LinkedList<SceneTurn> sturns = scene.getTurnList();

			for (SceneTurn t : sturns) {
				if (!speakersSet.contains(t.getSpeaker())) {
					speakersSet.add(t.getSpeaker());
				}
			}
		}
		return speakersSet;
	}

	private String getTimeMark() {
		String mark = "#TM" + mTimeMarkCnt++;
		return mark;
	}

	// Play some scene with the player
	@Override
	public final void play(final String name, final LinkedList<AbstractValue> args) {
		final Process process = ((Process) Thread.currentThread());
		final HashMap<String, String> mSceneParamMap = new HashMap<>();
		// Process The Arguments
		if ((args != null) && !args.isEmpty()) {
			// Get The First Argument
			final AbstractValue value = args.getFirst();
			// Check The Argument Type
			if (value.getType().equals(Type.STRUCT)) {
				// Cast The Argument Type
				final StructValue struct = (StructValue) value;
				// Process Scene Arguments
				for (final Entry<String, AbstractValue> entry : struct.getValueMap().entrySet()) {
					if (entry.getValue().getType() == Type.STRING) {
						mSceneParamMap.put(entry.getKey(), ((StringValue) entry.getValue()).getValue());
					} else {
						// Process Other Argument Types
					}
				}
			}
		}

		// Create The Player Task
		Task task = new Task(process.getName() + name + " StickManScenePlayer") {
			@Override
			public void run() {
				// Select The Scene
				final SceneScript script = mProject.getSceneScript();
				final SceneGroup group = script.getSceneGroup(name);
				final SceneObject scene = group.select();

				// Scene Visualization
				EventDispatcher.getInstance().convey(new SceneExecutedEvent(this, scene));
				// Process The Turns
				for (SceneTurn turn : scene.getTurnList()) {
					// Turn Visualization
					mLogger.message("Executing turn:" + turn.getText());
					EventDispatcher.getInstance().convey(new TurnExecutedEvent(this, turn));

					// Get The Turn Speaker
					final String speaker = turn.getSpeaker();

					if (speaker == null) {
						// Get The Default Speaker
					}

					// Count The Word Number
					int wordCount = 0;
					String alreadyUttered = "";

					// Process Utterance
					for (SceneUttr utt : turn.getUttrList()) {
						// create a new word time mark sequence based on the current utterance
						WordTimeMarkSequence wts = new WordTimeMarkSequence(utt.getCleanText());
						StickmanEventAction stickmanEventAction = new StickmanEventAction(StickmanStage.getStickman(speaker), 0, "environment", "Speaking", 3000, wts, false);
						mActionPlayer.addAction(stickmanEventAction);
						// add mouth open
						mActionPlayer.addAction(new StickmanAction(StickmanStage.getStickman(speaker), 0, "face", "Mouth_O", 200, "", true));
						// add mounth closed
						mActionPlayer.addAction(new StickmanAction(StickmanStage.getStickman(speaker), 190, "face", "Mouth_Default", 10, "", false));

						EventDispatcher.getInstance().convey(new UtteranceExecutedEvent(this, utt));

						// Process the words of this utterance
						// remember timemark
						String tm = getTimeMark();
						for (AbstractWord word : utt.getWordList()) {

							if (word instanceof SceneWord) {
								String w = ((SceneWord) word).getText();
								// add word to the word time mark sequence
								wts.add(new Word(w));
								// generate a new time mark after each word
								tm = getTimeMark();

								wordCount = w.length();

								alreadyUttered += w;
							} else if (word instanceof SceneParam) {
								// Visualization
								//mLogger.message("Executing param:" + ((SceneParam) word).getText());
							} else if (word instanceof ActionObject) {
								ActionObject ao = ((ActionObject) word);

								if (ao.getName().equalsIgnoreCase("waveleft")) {
									// give time mark to general event action
									stickmanEventAction.addTimeMark(tm);
									// create action
									StickmanAction sa = new StickmanAction(StickmanStage.getStickman(speaker), -1, "gesture", ao.getName(), 1000, "", false);
									// set time mark to action
									sa.setTimeMark(tm);
									// add time mark action to action player - to be played at the specfic timemark
									mActionPlayer.addTimeMarkAction(sa);
									// add time mark to word time mark sequence
									wts.add(new TimeMark(tm));
								}
//								if (ao.getName().equalsIgnoreCase("tiltleft")) {
//									mActionPlayer.addTimeMarkAction(new StickmanAction(StickmanStage.getStickman(speaker), TimingInfo.spokenStringDuration(alreadyUttered), "head", ao.getName(), 1000, "", true));
//								}
//								if (ao.getName().equalsIgnoreCase("tiltleftback")) {
//									mActionPlayer.addTimeMarkAction(new StickmanAction(StickmanStage.getStickman(speaker), TimingInfo.spokenStringDuration(alreadyUttered), "head", ao.getName(), 1000, "", true));
//								}
								if (ao.getName().equalsIgnoreCase("smile")) {
									stickmanEventAction.addTimeMark(tm);
									// create action
									StickmanAction sa = new StickmanAction(StickmanStage.getStickman(speaker), -1, "face", ao.getName(), 200, "", false);
									// set time mark to action
									sa.setTimeMark(tm);
									// add time mark action to action player - to be played at the specfic timemark
									mActionPlayer.addTimeMarkAction(sa);
									// add time mark to word time mark sequence
									wts.add(new TimeMark(tm));
								}
//								if (ao.getName().equalsIgnoreCase("sad")) {
//									mActionPlayer.addTimeMarkAction(new StickmanAction(StickmanStage.getStickman(speaker), TimingInfo.spokenStringDuration(alreadyUttered), "face", ao.getName(), 200, "", false));
//								}
							} else if (word instanceof SceneAbbrev) {
								// Visualization
								//mLogger.message("Executing abbreviation:" + ((SceneAbbrev) word).getText());
							}
						}

						// play all actions of an utterance
						mActionPlayer.play();

						// wait for all actions to be played
						try {
							mAllActionSync.acquire();
						} catch (InterruptedException ex) {
							mLogger.failure(ex.getMessage());
						}
					}

					// Exit if interrupted
					if (isDone()) {
						return;
					}
				}
			}
		};

		// Start the player task
		task.start();
		// Wait for termination
		boolean finished = false;
		while (!finished) {
			try {
				// Join the player task
				task.join();				
				// Stop waiting for task
				finished = true;
			} catch (final InterruptedException exc) {
				// Print some warning
				mLogger.warning("Warning: Interrupting process '" + process + "'");
				// Terminate the task
				task.abort();
				// Interrupt the task
				task.interrupt();

			}
		}
	}

	/*
	 private boolean init() {
	 // Initialize the delay
	 try {
	 // Check if the config is null
	 if (mConfig != null) {
	 mDelay = Integer.parseInt(
	 mConfig.getProperty("delay"));
	 // Print an error message in this case
	 mLogger.message("Initializing the default scene player delay with '" + mDelay + "'");
	 // Return failure if it does not exist
	 return true;
	 } else {
	 // Print an error message in this case
	 mLogger.warning("Warning: Cannot read bad default scene player configuration");
	 // Return failure if it does not exist
	 return false;
	 }
	 } catch (final NumberFormatException exc) {
	 // Print an error message in this case
	 mLogger.failure("Error: Cannot initialize the default scene player delay");
	 // Return failure if it does not exist
	 return false;
	 }
	 }
	 */
}
