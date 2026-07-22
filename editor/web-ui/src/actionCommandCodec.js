// Shared build/parse logic for a single action's bracket-body command text, e.g.
// "emotion type='happy' intensity='0.8'" — the same syntax ParameterEnvelopeEditor and
// App.svelte's ACTION_FEATURE_RE both read/write independently. Generalizes them into one
// schema-driven pair (build/parse) so ActionForm can render an arbitrary command's params
// without a bespoke editor component per action name.

// Mirrors App.svelte's ACTION_NAME_RE / ACTION_FEATURE_RE exactly (kept in sync, not imported,
// since App.svelte's copy also parses the surrounding actor/bracket wrapper this module never
// sees — this module only ever handles one already-unwrapped command body).
const NAME_RE = /^([A-Za-z_]\w*)/;
const FEATURE_RE = /([A-Za-z_]\w*)\s*=\s*(?:'([^']*)'|(-?\d+\.?\d*|[A-Za-z_]\w*))/g;

function escapeValue(rawValue) {
  return String(rawValue).replace(/\\/g, "\\\\").replace(/'/g, "\\'");
}

// True if `value` differs from the param's declared default (or there's no default to compare
// against) — i.e. whether it's worth writing out, mirroring ParameterEnvelopeEditor's own
// "only write what differs from default" rule (kept minimal on purpose, for readability).
function isNonDefault(value, param) {
  if (value === undefined || value === null || value === "") return false;
  if (!param || param.default === undefined || param.default === null) return true;
  return String(value) !== String(param.default);
}

// (actionName: string, values: {[paramName]: string}, schema: PluginCommand-shaped
// {name, params: [{name, default, required}]}) => string — e.g. "emotion type='happy'
// intensity='0.8'". Required params are always included (even if equal to their default);
// optional params are only included when they differ from their declared default, so authored
// scripts stay short. Returns "" (falsy — every caller gates Insert/Play on this) if any required
// param is still empty, rather than silently emitting an incomplete command missing it (e.g.
// "background" with no color at all) — reported 2026-07-22.
export function buildCommandText(actionName, values, schema) {
  const params = Array.isArray(schema?.params) ? schema.params : [];
  const parts = [actionName];
  for (const param of params) {
    const value = values?.[param.name];
    if (value === undefined || value === null || value === "") {
      if (param.required) return "";
      continue;
    }
    if (!param.required && !isNonDefault(value, param)) continue;
    parts.push(`${param.name}='${escapeValue(value)}'`);
  }
  return parts.join(" ");
}

// (text: string) => {name: string, values: {[paramName]: string}} | null — parses a single
// unwrapped command body (no surrounding brackets/actor) back into a flat value map, e.g. for
// re-populating ActionForm when editing an already-inserted command.
export function parseCommandBody(text) {
  const trimmed = String(text ?? "").trim();
  if (!trimmed) return null;
  const nameMatch = trimmed.match(NAME_RE);
  if (!nameMatch) return null;
  const name = nameMatch[1];
  const rest = trimmed.slice(nameMatch[0].length);
  const values = {};
  FEATURE_RE.lastIndex = 0;
  let match;
  while ((match = FEATURE_RE.exec(rest))) {
    values[match[1]] = match[2] !== undefined ? match[2] : match[3];
  }
  return { name, values };
}
