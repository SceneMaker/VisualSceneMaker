package de.dfki.vsm.runtime.player.factory;

import de.dfki.vsm.runtime.player.activity.AbstractActivity;
import de.dfki.vsm.runtime.player.context.AbstractContext;

/**
 * @author Gregor Mehlmann
 */
public interface AbstractFactory {

    public String compile(
            final AbstractActivity activity,
            final AbstractContext context);

}
