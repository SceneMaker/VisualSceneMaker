// Shared between InsertActionDialog (via ActionForm's emotion envelope widget and its
// convenience-alias fallback) and SiaPanel (turn-agnostic preview) so the known facial-expression
// vocabulary lives in exactly one place.
export const EMOTION_TYPES = [
  "happy", "sad", "angry", "tear", "disgust", "surprise",
  "smile", "excited", "fear", "bored", "relaxed"
];
