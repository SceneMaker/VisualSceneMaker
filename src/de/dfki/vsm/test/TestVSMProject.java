package de.dfki.vsm.test;

import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.ProjectEditor;
import java.io.File;

/**
 *
 * @author Not me
 */
public class TestVSMProject {

	public static void main(String args[]) {

		/*
		 // Create a new runtime project
		 final RunTimeProject rproject = new RunTimeProject();
		 rproject.parse(new File(args[0]));
		 RunTimeInstance.getInstance().launch(rproject);
		 //RunTimeInstance.getInstance().launch(rproject);
		 //RunTimeInstance.getInstance().unload(rproject);
		 //RunTimeInstance.getInstance().unload(rproject);
		 rproject.write(new File("_rt1"));
		 rproject.setProjectName("lala");
		 rproject.write(new File("_rt2"));
		 */
		final EditorInstance instance = EditorInstance.getInstance();
		instance.setVisible(true);

		try {
			Thread.sleep(5000);
		} catch (Exception exc) {
			// Do something here
		}

		instance.hideWelcomeStickman();

		final EditorProject project = new EditorProject();
		project.parse(new File("res/prj/test").getPath());

		final ProjectEditor editor = instance.showProject(project);

		try {
			Thread.sleep(5000);
		} catch (Exception exc) {
			// Do something here
		}

		instance.hideProject(editor);
	}
}
