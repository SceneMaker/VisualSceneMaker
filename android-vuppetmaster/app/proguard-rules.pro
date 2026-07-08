# Minimal, debug-friendly. Plugins are loaded reflectively by class name, so keep the
# executor and transport classes if minification is ever enabled.
-keep class de.dfki.vsm.xtension.charamelEmbed.** { *; }
-keep class de.dfki.vsm.runtime.** { *; }
