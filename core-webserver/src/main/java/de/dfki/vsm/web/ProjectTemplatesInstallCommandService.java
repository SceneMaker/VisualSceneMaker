package de.dfki.vsm.web;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.net.URL;
import java.util.Enumeration;

/**
 * Handles Project.Templates.Install command.
 */
public final class ProjectTemplatesInstallCommandService {

    public interface Context {
        JSONObject errorResponse(String code, String message);

        boolean hasRuntimeProject(String projectId);

        String projectPath(String projectId);

        JSONObject pluginSpec(String className);

        ClassLoader classLoader();

        void copyTemplateDirectory(URL resourceUrl, String basePath, File destDir, JSONArray createdFiles, JSONArray skippedFiles);

        void warn(String message);
    }

    public JSONObject dispatch(final JSONObject params, final Context context) {
        String pid = params.optString("projectId", "");
        String pluginClassName = params.optString("className", "").trim();

        if (!context.hasRuntimeProject(pid)) {
            return context.errorResponse("PROJECT_NOT_FOUND", "Project not found");
        }
        if (pluginClassName.isEmpty()) {
            return context.errorResponse("BAD_REQUEST", "Missing className");
        }

        String projectPath = context.projectPath(pid);
        if (projectPath == null || projectPath.isEmpty()) {
            return context.errorResponse("PROJECT_PATH_UNKNOWN", "Project path not available");
        }
        File projectDir = new File(projectPath);
        if (!projectDir.isDirectory()) {
            return context.errorResponse("PROJECT_DIR_INVALID", "Project directory not found");
        }

        JSONObject pluginSpec = context.pluginSpec(pluginClassName);
        if (pluginSpec == null) {
            return noTemplatesResponse("No templates defined for this plugin");
        }

        JSONObject templates = pluginSpec.optJSONObject("templates");
        if (templates == null) {
            return noTemplatesResponse("No templates defined for this plugin");
        }

        String resourcePath = templates.optString("resourcePath", "templates/");
        JSONArray targetDirs = templates.optJSONArray("targetDirs");
        if (targetDirs == null || targetDirs.isEmpty()) {
            return noTemplatesResponse("No target directories specified");
        }

        JSONArray createdFiles = new JSONArray();
        JSONArray skippedFiles = new JSONArray();
        ClassLoader cl = context.classLoader();

        for (int i = 0; i < targetDirs.length(); i++) {
            String targetDir = targetDirs.optString(i, "").trim();
            if (targetDir.isEmpty()) {
                continue;
            }

            File destDir = new File(projectDir, targetDir);
            if (!destDir.exists()) {
                destDir.mkdirs();
                createdFiles.put(targetDir + "/");
            }

            String templatePath = resourcePath + targetDir + "/";
            try {
                Enumeration<URL> resources = cl.getResources(templatePath);
                while (resources.hasMoreElements()) {
                    URL resourceUrl = resources.nextElement();
                    context.copyTemplateDirectory(resourceUrl, templatePath, destDir, createdFiles, skippedFiles);
                }
            } catch (Exception ex) {
                context.warn("Failed to extract templates from " + templatePath + ": " + ex.getMessage());
            }
        }

        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("createdFiles", createdFiles);
        response.put("skippedFiles", skippedFiles);
        return response;
    }

    private JSONObject noTemplatesResponse(final String message) {
        JSONObject response = new JSONObject();
        response.put("status", "ok");
        response.put("createdFiles", new JSONArray());
        response.put("message", message);
        return response;
    }
}
