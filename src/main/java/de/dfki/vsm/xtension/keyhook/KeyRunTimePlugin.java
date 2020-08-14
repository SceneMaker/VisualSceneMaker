package de.dfki.vsm.xtension.keyhook;

import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.extensionAPI.ExportableCompletion;
import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
import de.dfki.vsm.util.log.LOGConsoleLogger;
import de.dfki.vsm.util.tts.marytts.MaryTTsProcess;
import de.dfki.vsm.xtension.keyhook.util.KeyRunTimePluginProperty;
import org.jnativehook.GlobalScreen;
import org.jnativehook.NativeHookException;
import org.jnativehook.keyboard.NativeKeyEvent;
import org.jnativehook.keyboard.NativeKeyListener;
import org.jnativehook.mouse.NativeMouseEvent;
import org.jnativehook.mouse.NativeMouseListener;
import org.jnativehook.mouse.NativeMouseMotionListener;

import java.util.List;
import java.util.Map;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class KeyRunTimePlugin extends RunTimePlugin implements ExportableProperties, ExportableCompletion {

    // The singelton logger instance
    private final LOGConsoleLogger mLogger = LOGConsoleLogger.getInstance();

    // properties
    private ExportableProperties exportableProperties = new KeyRunTimePluginProperty();
    private ExportableCompletion exportableActions = null;

    // The native key listener
    GlobalKeyListener mKeyListener = new GlobalKeyListener();
    GlobalMouseListener mMouseListener = new GlobalMouseListener();
    GlobalMouseMotionListener mMouseMotionListener = new GlobalMouseMotionListener();

    // the dummy mouse listener classes

    public class GlobalMouseListener implements NativeMouseListener {

        @Override
        public void nativeMouseClicked(NativeMouseEvent nativeMouseEvent) {

        }

        @Override
        public void nativeMousePressed(NativeMouseEvent nativeMouseEvent) {

        }

        @Override
        public void nativeMouseReleased(NativeMouseEvent nativeMouseEvent) {

        }
    }

    public class GlobalMouseMotionListener implements NativeMouseMotionListener {

        @Override
        public void nativeMouseMoved(NativeMouseEvent nativeMouseEvent) {
            //
        }

        @Override
        public void nativeMouseDragged(NativeMouseEvent nativeMouseEvent) {

        }
    }

    // The key listener class        
    public class GlobalKeyListener implements NativeKeyListener {

        public void nativeKeyPressed(final NativeKeyEvent e) {
            final String code = NativeKeyEvent.getKeyText(e.getKeyCode());
            
            //mLogger.message("Pressed Key: " + code);

            //if (mProject.hasVariable())

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
        GlobalScreen.addNativeMouseListener(mMouseListener);
        mMouseListener = new GlobalMouseListener();
        GlobalScreen.addNativeMouseMotionListener(mMouseMotionListener);
        mMouseMotionListener = new GlobalMouseMotionListener();
    }

    private void showMissingVariable() {
        WaitingDialog InfoDialog = new WaitingDialog("Loading MaryTTS...");

        InfoDialog.setModal(true);
        InfoDialog.setVisible(true);
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

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableProperties() {
        return exportableProperties.getExportableProperties();
    }

    @Override
    public Map<ProjectProperty, ProjectValueProperty> getExportableAgentProperties() {
        return exportableProperties.getExportableAgentProperties();
    }

    @Override
    public List<String> getExportableActions() {
        return exportableActions.getExportableActions();
    }
}
