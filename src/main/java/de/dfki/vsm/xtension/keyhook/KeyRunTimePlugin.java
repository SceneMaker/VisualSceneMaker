package de.dfki.vsm.xtension.keyhook;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class KeyRunTimePlugin extends RunTimePlugin {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // The native key listener
    GlobalKeyListener mKeyListener = new GlobalKeyListener();

    // The key listener class        
    public class GlobalKeyListener implements NativeKeyListener {

        public void nativeKeyPressed(final NativeKeyEvent e) {
            final String code = NativeKeyEvent.getKeyText(e.getKeyCode());
            
            mLogger.message("Pressed Key: " + code);
            
            mProject.setVariable("PressedKey", code);

            if (e.getKeyCode() == NativeKeyEvent.VC_ESCAPE) {
                //GlobalScreen.unregisterNativeHook();
            }
        }

        public void nativeKeyReleased(NativeKeyEvent e) {
            //System.out.println("Key Released: " + NativeKeyEvent.getKeyText(e.getKeyCode()));
        }

        public void nativeKeyTyped(NativeKeyEvent e) {
            //System.out.println("Key Typed: " + e.getKeyText(e.getKeyCode()));
        }
    }

    public KeyRunTimePlugin(
            final PluginConfig config,
            final RunTimeProject project) {
        // Initialize the plugin
        super(config, project);
    }

    // Launch plugin
    @Override
    public void launch() {
        mLogger.message("Launching KeyRunTimePlugin ...");
        try {
            GlobalScreen.registerNativeHook();
        } catch (NativeHookException ex) {
            mLogger.failure("There was a problem registering the native key hook.");
            mLogger.failure(ex.getMessage());
            System.exit(1);
        }
        GlobalScreen.addNativeKeyListener(mKeyListener);
        mKeyListener = new GlobalKeyListener();
    }

    // Unload plugin
    @Override
    public void unload() {

        try {
            GlobalScreen.unregisterNativeHook();
        } catch (NativeHookException e) {
            mLogger.failure("There was a problem unregistering the native key hook.");
            mLogger.failure(e.getMessage());
        }
        GlobalScreen.removeNativeKeyListener(mKeyListener);
    }
}
