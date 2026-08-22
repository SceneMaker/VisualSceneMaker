/**
 * Path-addressed operations over a screen's element tree.
 *
 * A screen's elements are a plain array whose entries may themselves carry a `children` array
 * (today only `vsm-panel` does, but nothing here assumes any particular type). A path is an
 * array of indices describing how to reach a node: `[i]` is `elements[i]`; `[i, j]` is
 * `elements[i].children[j]`; and so on to any depth. This is what lets ScreenEditor.svelte's
 * property panel and element list work the same way regardless of how deep an element is
 * nested, instead of the two hard-coded tiers (top-level `elements[i]` / one level of
 * `children[j]`) it grew from.
 *
 * The empty path `[]` means "the root container itself" — valid as the *container* argument to
 * insertChildAtPath (to add a top-level element) but not a valid element path for the other
 * operations, which all address an existing element and are no-ops on `[]`.
 *
 * Every function is pure: it returns a new elements array and never mutates its input or any
 * node inside it, matching how the rest of the editor already treats parsedSchema
 * (replace-and-reassign, so Svelte's reactivity notices the change).
 */

/** A path's identity as a Set/Map key — e.g. for tracking which rows are expanded. */
export function pathKey(path) {
    return path.join(",");
}

function childrenOf(node) {
    return node.children ?? [];
}

function withChildren(node, children) {
    return { ...node, children };
}

/** The element at `path`, or undefined if the path doesn't resolve. */
export function getAtPath(elements, path) {
    if (path.length === 0) return undefined;
    let node = elements[path[0]];
    for (let depth = 1; depth < path.length && node; depth++) {
        node = childrenOf(node)[path[depth]];
    }
    return node;
}

/**
 * The array that directly contains the element at `path` — `elements` itself for a top-level
 * path, or some ancestor's `children` array otherwise. `path` must be non-empty. Useful for
 * bounds checks (sibling count, "is this the last child") without duplicating the walk.
 */
export function getContainerAtPath(elements, path) {
    if (path.length === 0) return undefined;
    if (path.length === 1) return elements;
    const parent = getAtPath(elements, path.slice(0, -1));
    return parent ? childrenOf(parent) : undefined;
}

/**
 * Returns a new elements tree with the node at `path` replaced by `update(currentNode)`.
 * Every mutation below is built on this — it is the one place that walks down and rebuilds new
 * arrays/objects back up, so a caller several levels deep never mutates a shared ancestor.
 * No-op (returns `elements` unchanged) if `path` is empty or doesn't resolve.
 */
function replaceAtPath(elements, path, update) {
    if (path.length === 0) return elements;
    const [head, ...rest] = path;
    const node = elements[head];
    if (node === undefined) return elements;
    const updated = rest.length === 0
        ? update(node)
        : withChildren(node, replaceAtPath(childrenOf(node), rest, update));
    const next = [...elements];
    next[head] = updated;
    return next;
}

/** Sets one property on the element at `path`; deletes it instead when `value` is undefined. */
export function setPropAtPath(elements, path, key, value) {
    return replaceAtPath(elements, path, (node) => {
        const next = { ...node };
        if (value === undefined) delete next[key];
        else next[key] = value;
        return next;
    });
}

/**
 * Sets one entry in the element's `style` object; deletes it when `value` is `""` or undefined,
 * and drops `style` entirely once it would be empty — mirroring setProp's delete-when-undefined
 * convention one level down.
 */
export function setStylePropAtPath(elements, path, key, value) {
    return replaceAtPath(elements, path, (node) => {
        const style = { ...(node.style ?? {}) };
        if (value === "" || value === undefined) delete style[key];
        else style[key] = value;
        const next = { ...node };
        if (Object.keys(style).length) next.style = style;
        else delete next.style;
        return next;
    });
}

/** Removes the element at `path`. No-op if `path` is empty. */
export function removeAtPath(elements, path) {
    if (path.length === 0) return elements;
    const index = path[path.length - 1];
    if (path.length === 1) return elements.filter((_, i) => i !== index);
    return replaceAtPath(elements, path.slice(0, -1), (node) =>
        withChildren(node, childrenOf(node).filter((_, i) => i !== index)));
}

/**
 * Appends `child` to the children of the container at `containerPath` — or to the root
 * elements array when `containerPath` is `[]`. `containerPath` names the *container*, not the
 * new child, so pass an existing panel's own path to add into it, or `[]` for a new top-level
 * element.
 */
export function insertChildAtPath(elements, containerPath, child) {
    if (containerPath.length === 0) return [...elements, child];
    return replaceAtPath(elements, containerPath, (node) =>
        withChildren(node, [...childrenOf(node), child]));
}

/**
 * Swaps the element at `path` with the sibling `direction` away (+1 or -1). No-op if that
 * sibling doesn't exist (already first/last) or `path` is empty.
 */
export function moveAtPath(elements, path, direction) {
    if (path.length === 0) return elements;
    const index = path[path.length - 1];
    const targetIndex = index + direction;
    const siblings = getContainerAtPath(elements, path);
    if (!siblings || targetIndex < 0 || targetIndex >= siblings.length) return elements;
    const reordered = [...siblings];
    [reordered[index], reordered[targetIndex]] = [reordered[targetIndex], reordered[index]];
    const parentPath = path.slice(0, -1);
    if (parentPath.length === 0) return reordered;
    return replaceAtPath(elements, parentPath, (node) => withChildren(node, reordered));
}
