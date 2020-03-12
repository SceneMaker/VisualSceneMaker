package de.dfki.vsm.xtension.androidGui;

import java.util.function.Consumer;

public class AndroidTextField {
    public Consumer<String> textChanged;
    public Consumer<String> setText;

    public AndroidTextField(Consumer<String> textChanged, Consumer<String> setText) {
        this.textChanged = textChanged;
        this.setText = setText;
    }
}
