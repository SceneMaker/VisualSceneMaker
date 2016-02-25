//package de.dfki.vsm.api;
//
//import de.dfki.vsm.model.scenescript.AbstractWord;
//import de.dfki.vsm.model.scenescript.SceneGroup;
//import de.dfki.vsm.model.scenescript.SceneObject;
//import de.dfki.vsm.model.scenescript.SceneScript;
//import de.dfki.vsm.model.scenescript.SceneTurn;
//import de.dfki.vsm.model.scenescript.SceneUttr;
//import de.dfki.vsm.runtime.project.RunTimeProject;
//import de.dfki.vsm.runtime.values.AbstractValue;
//import java.util.HashMap;
//import java.util.LinkedList;
//
///**
// * @author Gregor Mehlmann
// */
//public final class EmpaTScenePlayer extends EventBasedPlayer {
//
//    // The runtime project data
//    private RunTimeProject mProject;
//
//    // Construct the scene player 
//    private EmpaTScenePlayer() {
//        // Do nothing here
//    }
//    // The singelton player instance
//    public static EmpaTScenePlayer sInstance;
//
//    // Create the singelton instance
//    public static synchronized EmpaTScenePlayer getInstance() {
//        if (sInstance == null) {
//            sInstance = new EmpaTScenePlayer();
//        }
//        return sInstance;
//    }
//
//    @Override
//    public boolean launch(final RunTimeProject project) {
//        // Initialize project data
//        mProject = project;
//        // Return true at success
//        return true;
//    }
//
//    @Override
//    public boolean unload() {
//        // Unload the parent player
//        if (super.unload()) {
//
//            // Return true at success
//            return true;
//        }
//        // Return false at failure
//        return false;
//    }
//
//    @Override
//    public final void play(final String name, final LinkedList<AbstractValue> args) {
//        final HashMap<String, String> map = new HashMap<>();
//
//        final Task task = new Task(name) {
//
//            @Override
//            public void run() {
//                // Get the script from project
//                final SceneScript sceneScript = mProject.getSceneScript();
//                // Select default scene group
//                final SceneGroup sceneGroup = sceneScript.getSceneGroup(name);
//                // Select language dependency
//                final SceneGroup langGroup = sceneScript.getSceneGroup("de", name);
//                // Select a scene from group
//                final SceneObject selectedScene
//                        = (langGroup != null)
//                        ? (langGroup.select())
//                        : (sceneGroup.select());
//                // Get the final scene language
//                final String selectedLang = selectedScene.getLanguage();
//                // Process all turns of the scene 
//                for (final SceneTurn sceneTurn : selectedScene.getTurnList()) {
//                    // Get the speaker name of the turn 
//                    final String speakerName = sceneTurn.getSpeaker();
//                    // Process all utterances of the turn 
//                    for (final SceneUttr sceneUtt : sceneTurn.getUttrList()) {
//                        // A text builder for the utterance
//                        final StringBuilder textBuilder = new StringBuilder();
//                        // The word count of the utterance
//                        final int wordCount = sceneUtt.getWordList().size();
//                        // Check if we have at least a word
//                        if (wordCount > 0) {
//                            // Get the last word of the utterance
//                            final AbstractWord lastWord = sceneUtt.getWordList().getLast();
//                            // Process all words of the utterance
//                            for (final AbstractWord sceneWord : sceneUtt.getWordList()) {
//                                // Append the text representation of the
//                                // word to the builder while eventually
//                                // replacing variables with their values
//                                textBuilder.append(sceneWord.getText(map));
//                                // Append a whitespace after each word 
//                                // with the exception of the last word
//                                if (!sceneWord.equals(lastWord)) {
//                                    textBuilder.append(' ');
//                                }
//                            }
//                            // Set Punctation mark if we have
//                            // some text that has to be spoken
//                            if (textBuilder.length() > 0) {
//                                textBuilder.append(sceneUtt.getPunct());
//                                
//                                
//                                
//                            }
//                        }
//                        // Exit if interrupted
//                        if (isDone()) {
//                            return;
//                        }
//                    }
//                    // Exit if interrupted
//                    if (isDone()) {
//                        return;
//                    }
//                }
//            }
//        };
//        // Start the player task
//        task.start();
//        // Wait for termination
//        boolean finished = false;
//        while (!finished) {
//            try {
//                // Join the player task
//                task.join();
//                // Finish this execution
//                finished = true;
//            } catch (final InterruptedException exc) {
//                // Abort the player task
//                task.abort();
//            }
//        }
//    }
//}
