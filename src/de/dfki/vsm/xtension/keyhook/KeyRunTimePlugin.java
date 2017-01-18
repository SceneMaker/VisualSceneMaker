package de.dfki.vsm.xtension.keyhook;

import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;

/**
 * @author Gregor Mehlmann
 */
public final class KeyRunTimePlugin extends RunTimePlugin {

    // The native key listener
    final GlobalKeyListener mKeyListener = new GlobalKeyListener();

    // The key listener class        
    public class GlobalKeyListener implements NativeKeyListener {

        public void nativeKeyPressed(final NativeKeyEvent e) {
            final String code = NativeKeyEvent.getKeyText(e.getKeyCode());
            
            System.out.println("Pressed Key: " + code);
            
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
        System.err.println("Launchin key hook plugin");
        try {
            GlobalScreen.registerNativeHook();
        } catch (NativeHookException ex) {
            System.err.println("There was a problem registering the native hook.");
            System.err.println(ex.getMessage());
            System.exit(1);
        }
        GlobalScreen.addNativeKeyListener(mKeyListener);

    }

    // Unload plugin
    @Override
    public void unload() {
        GlobalScreen.removeNativeKeyListener(mKeyListener);
    }
}
