/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package de.dfki.vsm.players.stickman.util;

import de.dfki.vsm.model.scenescript.SceneUttr;
import de.dfki.vsm.players.action.sequence.WordTimeMarkSequence;
import de.dfki.vsm.players.stickman.Stickman;
import de.dfki.vsm.players.stickman.animation.Animation;
import de.dfki.vsm.players.stickman.animation.EventAnimation;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

/**
 *
 * @author Patrick Gebhard
 *
 */
public class AnimationLoader {

	private final static String SANIMATIONPATH = "de.dfki.vsm.players.stickman.animation.";
	private static AnimationLoader sInstance = null;

	private AnimationLoader() {
	}

	public static AnimationLoader getInstance() {
		if (sInstance == null) {
			sInstance = new AnimationLoader();
		}

		return sInstance;
	}

	public Animation load(Stickman sm, String type, String name, int duration, boolean block) {
		Animation a = null;

		try {
			Class c = Class.forName(SANIMATIONPATH + type + "." + name);
			Constructor[] constructors = c.getConstructors();
			for (Constructor con : constructors) {
				Class[] params = con.getParameterTypes();

				if (params.length == 3) {
					if (params[0].getSimpleName().equalsIgnoreCase("stickman") && params[1].getSimpleName().equalsIgnoreCase("int")
					  && params[2].getSimpleName().equalsIgnoreCase("boolean")) {
						a = (Animation) c.getDeclaredConstructor(params).newInstance(sm, duration, block);
					}
				}

			}
		} catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
			sm.mLogger.severe("Animation type \"" + type + "\" name \"" + name + "\" cannot be found in " + SANIMATIONPATH + type + "." + name);
		}

		return a;
	}

	public EventAnimation load(Stickman sm, String type, String name, WordTimeMarkSequence wts, int duration, boolean block) {
		EventAnimation a = null;

		try {
			Class c = Class.forName(SANIMATIONPATH + type + ".event." + name);

			// TODO - Hier weiter ...
			Constructor[] constructors = c.getConstructors();
			for (Constructor con : constructors) {
				Class[] params = con.getParameterTypes();

				if (params.length == 4) {
					if (params[0].getSimpleName().equalsIgnoreCase("stickman") && params[1].getSimpleName().equalsIgnoreCase("WordTimeMarkSequence")
					  && params[2].getSimpleName().equalsIgnoreCase("int")
					  && params[3].getSimpleName().equalsIgnoreCase("boolean")) {
						a = (EventAnimation) c.getDeclaredConstructor(params).newInstance(sm, wts, duration, block);
					}
				}

			}
		} catch (ClassNotFoundException | NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
			sm.mLogger.severe("Event animation type \"" + type + "\" name \"" + name + "\" cannot be found in " + SANIMATIONPATH + type + "." + name);
		}

		return a;
	}
}
