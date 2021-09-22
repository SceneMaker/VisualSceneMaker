package de.dfki.vsm.xtension.keyhook;

import de.dfki.vsm.editor.dialog.WaitingDialog;
import de.dfki.vsm.extensionAPI.ExportableCompletion;
import de.dfki.vsm.extensionAPI.ExportableProperties;
import de.dfki.vsm.extensionAPI.ProjectProperty;
import de.dfki.vsm.extensionAPI.value.ProjectValueProperty;
import de.dfki.vsm.model.project.PluginConfig;
import de.dfki.vsm.runtime.plugin.RunTimePlugin;
import de.dfki.vsm.runtime.project.RunTimeProject;
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

import java.util.logging.Logger ;
import java.util.logging.Level ;

/**
 * @author Gregor Mehlmann, Patrick Gebhard
 */
public final class KeyRunTimePlugin extends RunTimePlugin implements ExportableProperties, ExportableCompletion {

    // Properties
    private final ExportableProperties exportableProperties = new KeyRunTimePluginProperty();
    private final ExportableCompletion exportableActions = null;

    // Project variables
    private String mKeyPressedVarName ;

    // The native key listener
    private final GlobalKeyListener mKeyListener = new GlobalKeyListener();
    private final GlobalMouseListener mMouseListener = new GlobalMouseListener();
    private final GlobalMouseMotionListener mMouseMotionListener = new GlobalMouseMotionListener();

    // Mouse listener classes
    public static class GlobalMouseListener implements NativeMouseListener {

        @Override
        public void nativeMouseClicked(NativeMouseEvent nativeMouseEvent) { }

        @Override
        public void nativeMousePressed(NativeMouseEvent nativeMouseEvent) { }

        @Override
        public void nativeMouseReleased(NativeMouseEvent nativeMouseEvent) { }
    }

    public static class GlobalMouseMotionListener implements NativeMouseMotionListener {

        @Override
        public void nativeMouseMoved(NativeMouseEvent nativeMouseEvent) { }

        @Override
        public void nativeMouseDragged(NativeMouseEvent nativeMouseEvent) { }
    }

    // The key listener class        
    public class GlobalKeyListener implements NativeKeyListener {

        public void nativeKeyPressed(final NativeKeyEvent e) {
            final String code = NativeKeyEvent.getKeyText(e.getKeyCode());

            if (mProject.hasVariable(mKeyPressedVarName)) {
                mProject.setVariable(mKeyPressedVarName, code);
            } else {
                mLogger.warning("Can not set Pressed key value: project variable '" + mKeyPressedVarName + " is missing.");
            }

        }

        public void nativeKeyReleased(NativeKeyEvent e) { }

        public void nativeKeyTyped(NativeKeyEvent e) { }
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

        mKeyPressedVarName = mConfig.getProperty(KeyRunTimePluginProperty.PRESSED_KEY_VAR_NAME, KeyRunTimePluginProperty.PRESSED_KEY_VAR_DEFAULT) ;

        mLogger.message("Launching KeyRunTimePlugin ...");
        try {
            GlobalScreen.registerNativeHook();
        } catch (NativeHookException ex) {
            mLogger.failure("There was a problem registering the native key hook.");
            mLogger.failure(ex.getMessage());
            System.exit(1);
        }
        GlobalScreen.addNativeKeyListener(mKeyListener);
        GlobalScreen.addNativeMouseListener(mMouseListener);
        GlobalScreen.addNativeMouseMotionListener(mMouseMotionListener);

        // Temporarily fixes issue #227
        // Can be removed as son as jnativehook v2.2.0 is available on Maven
        // See: https://github.com/kwhat/jnativehook/issues/291
        Logger logger = Logger.getLogger(GlobalScreen.class.getPackage().getName());
        logger.setLevel(Level.WARNING);
        logger.setUseParentHandlers(false);
    }


    // Unload plugin
    @Override
    public void unload() {
        GlobalScreen.removeNativeKeyListener(mKeyListener);
        GlobalScreen.removeNativeMouseListener(mMouseListener);
        GlobalScreen.removeNativeMouseMotionListener(mMouseMotionListener);

        try {
            GlobalScreen.unregisterNativeHook();
        } catch (NativeHookException e) {
            mLogger.failure("There was a problem unregistering the native key hook.");
            mLogger.failure(e.getMessage());
        }
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
