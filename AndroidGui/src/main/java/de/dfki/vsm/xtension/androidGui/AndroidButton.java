package de.dfki.vsm.xtension.androidGui;

import java.util.function.Consumer;

public class AndroidButton {
    public Consumer<Void> onClick;
    public Consumer<String> setText;

    public AndroidButton(Consumer<Void> onClick, Consumer<String> setText) {
        this.onClick = onClick;
        this.setText = setText;
    }
}
