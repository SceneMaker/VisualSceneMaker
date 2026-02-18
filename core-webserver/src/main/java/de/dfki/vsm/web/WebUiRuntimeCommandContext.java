package de.dfki.vsm.web;

import de.dfki.vsm.runtime.project.RunTimeProject;
import org.json.JSONObject;

import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Adapter wiring WebUiServer state/actions into RuntimeCommandService.Context.
 */
final class WebUiRuntimeCommandContext implements RuntimeCommandService.Context {

    @FunctionalInterface
    interface RuntimeVariableSetter {
        JSONObject apply(String projectId, String name, String valueExpr);
    }

    private final Function<String, Boolean> loadProject;
    private final Supplier<String> firstLoadedProjectId;
    private final Function<String, RunTimeProject> runtimeProject;
    private final Function<String, String> runtimeState;
    private final BiConsumer<String, String> setRuntimeState;
    private final Function<String, String> projectPath;
    private final Function<String, String> projectName;
    private final Consumer<String> removeProject;
    private final BiFunction<String, String, JSONObject> errorResponse;
    private final Consumer<JSONObject> addRuntimeCapabilities;
    private final Consumer<String> log;
    private final RuntimeVariableSetter runtimeVariableSet;
    private final BiFunction<String, String, JSONObject> runtimeQuery;

    WebUiRuntimeCommandContext(
            final Function<String, Boolean> loadProject,
            final Supplier<String> firstLoadedProjectId,
            final Function<String, RunTimeProject> runtimeProject,
            final Function<String, String> runtimeState,
            final BiConsumer<String, String> setRuntimeState,
            final Function<String, String> projectPath,
            final Function<String, String> projectName,
            final Consumer<String> removeProject,
            final BiFunction<String, String, JSONObject> errorResponse,
            final Consumer<JSONObject> addRuntimeCapabilities,
            final Consumer<String> log,
            final RuntimeVariableSetter runtimeVariableSet,
            final BiFunction<String, String, JSONObject> runtimeQuery) {
        this.loadProject = Objects.requireNonNull(loadProject, "loadProject");
        this.firstLoadedProjectId = Objects.requireNonNull(firstLoadedProjectId, "firstLoadedProjectId");
        this.runtimeProject = Objects.requireNonNull(runtimeProject, "runtimeProject");
        this.runtimeState = Objects.requireNonNull(runtimeState, "runtimeState");
        this.setRuntimeState = Objects.requireNonNull(setRuntimeState, "setRuntimeState");
        this.projectPath = Objects.requireNonNull(projectPath, "projectPath");
        this.projectName = Objects.requireNonNull(projectName, "projectName");
        this.removeProject = Objects.requireNonNull(removeProject, "removeProject");
        this.errorResponse = Objects.requireNonNull(errorResponse, "errorResponse");
        this.addRuntimeCapabilities = Objects.requireNonNull(addRuntimeCapabilities, "addRuntimeCapabilities");
        this.log = Objects.requireNonNull(log, "log");
        this.runtimeVariableSet = Objects.requireNonNull(runtimeVariableSet, "runtimeVariableSet");
        this.runtimeQuery = Objects.requireNonNull(runtimeQuery, "runtimeQuery");
    }

    @Override
    public boolean loadProject(final String path) {
        return loadProject.apply(path);
    }

    @Override
    public String firstLoadedProjectId() {
        return firstLoadedProjectId.get();
    }

    @Override
    public RunTimeProject runtimeProject(final String projectId) {
        return runtimeProject.apply(projectId);
    }

    @Override
    public String runtimeState(final String projectId) {
        return runtimeState.apply(projectId);
    }

    @Override
    public void setRuntimeState(final String projectId, final String state) {
        setRuntimeState.accept(projectId, state);
    }

    @Override
    public String projectPath(final String projectId) {
        return projectPath.apply(projectId);
    }

    @Override
    public String projectName(final String projectId) {
        return projectName.apply(projectId);
    }

    @Override
    public void removeProject(final String projectId) {
        removeProject.accept(projectId);
    }

    @Override
    public JSONObject errorResponse(final String code, final String message) {
        return errorResponse.apply(code, message);
    }

    @Override
    public void addRuntimeCapabilities(final JSONObject target) {
        addRuntimeCapabilities.accept(target);
    }

    @Override
    public void log(final String message) {
        log.accept(message);
    }

    @Override
    public JSONObject runtimeVariableSet(final String projectId, final String name, final String valueExpr) {
        return runtimeVariableSet.apply(projectId, name, valueExpr);
    }

    @Override
    public JSONObject runtimeQuery(final String projectId, final String query) {
        return runtimeQuery.apply(projectId, query);
    }
}
