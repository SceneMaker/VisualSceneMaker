# Plugin Descriptor Generator

This document describes how to generate and maintain `sceneflow-interface.json`
files for plugins.

## What it does

The Gradle task `generatePluginDescriptors` scans Java sources under `plugins/`
and creates or updates `sceneflow-interface.json` per plugin folder. It infers:

- Commands (action names) from `equalsIgnoreCase(...)` and `switch (...)` cases.
- Parameters from `activity.get("...")` and `getActionFeatureValue("...")`.
- Variables written from `mProject.setVariable(...)`.
- Plugin config keys from `mConfig.getProperty("...")`.
- Categories (`input`, `output`, `processing`, `mixed`) from simple heuristics.

All inferred data is marked with `"inferred": true` where applicable.

## Basic usage

Generate draft descriptors for all plugins:

```
./gradlew generatePluginDescriptors
```

If a descriptor exists and contains:

```
"generated": false
```

the task will skip it unless forced.

Force overwrite all descriptors:

```
./gradlew generatePluginDescriptors -PforceDescriptors=true
```

## Partial updates (recommended after manual edits)

Update only specific sections while preserving everything else:

```
./gradlew generatePluginDescriptors -PupdateDescriptorSections=commands,writes,reads,config
```

Notes:
- This ignores `"generated": false` for the listed sections only.
- If the file does not exist, a full descriptor is created.

## Manual edit workflow

1) Run the generator once.
2) Edit the JSON to add types, descriptions, examples, and correct categories.
3) Set `"generated": false` to prevent full overwrites.
4) Use partial updates to refresh only machine-derived sections.

## Output location

Descriptors are written to:

```
plugins/<plugin>/sceneflow-interface.json
```
