## Swing parity audit
This checklist compares Swing dialogs/features with the current Web UI.

### Dialog parity
- AboutDialog: missing (no About panel in Web UI).
- CmdDialog: covered (command executions list + command dialog).
- CreateAltStartNodeDialog: covered (alt-start text field in edge inspector).
- CreateExpDialog: covered (inline expression/condition inputs in edge inspector).
- ErrorDialog: partial (inline error banners only, no modal dialog).
- FunDefDialog: deferred (LLM redesign).
- MemberDefDialog: covered (struct member editor in type definitions dialog).
- ModifyCEdgeDialog: covered (edge inspector).
- ModifyIEdgeDialog: covered (edge inspector).
- ModifyPEdgeDialog: covered (edge inspector + probability manager).
- ModifyTEdgeDialog: covered (edge inspector + timeout spec).
- MonitorDialog: missing (runtime monitor window).
- NewProjectDialog: covered (landing page new project panel).
- OptionsDialog: covered (preferences dialog).
- QuitDialog: missing (no quit confirmation dialog).
- SaveFileDialog: covered (landing page save-as panel + editor Save/Save As).
- SceneActionDialog: deferred (LLM redesign).
- SelectPlayerDialog: missing (no missing-agent/player wizard).
- TypeDefDialog: covered (type definition editor dialog).
- VarDefDialog: covered (variable definition editor dialog).
- WaitingDialog: missing (no blocking progress dialog).

### Other parity notes
- Function definitions (Glue command definitions) are deferred for LLM redesign.
- Scene action (Acticon) editing is deferred for LLM redesign.

### Future (LLM redesign)
- Function definition editor (Glue commands).
- Scene Action editor (Acticon actions).
- Runtime monitor window is not represented; only status + activity markers exist.
- Inspector metadata fields removed intentionally (ID/Type/Flavor/History) per UX scope.
