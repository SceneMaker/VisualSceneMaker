package de.dfki.vsm.xtension.util.plugin;

public interface Plugin {
    void start();

    String marker(long id);

    void stop();
}
