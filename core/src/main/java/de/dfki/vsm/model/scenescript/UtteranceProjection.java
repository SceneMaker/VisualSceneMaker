package de.dfki.vsm.model.scenescript;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The spoken-text projection of one {@link SceneUttr}: the utterance with all inline behavior
 * commands removed, plus a bidirectional offset map back to the original script text and the
 * position each removed command occupied.
 *
 * <h2>Why this exists</h2>
 * A {@code SceneUttr} interleaves spoken words with {@link ActionObject} commands. Sending the raw
 * line to a dependency parser lets the commands perturb the parse — verified: the same utterance
 * parses differently with and without its brackets, which is fatal for learning where authors place
 * commands, because the label would be perturbing its own features. So the parser sees
 * {@link #getCleanText()}, and spans come back through {@link #toScriptSpan(int, int)}.
 *
 * <p>Deliberately <em>not</em> built on {@link SceneUttr#getCleanText()}, which keeps only
 * {@link SceneWord} elements and therefore drops {@link SceneParam} placeholders ({@code $user})
 * and {@link SceneAbbrev} contractions. Dropping those would change the sentence the parser sees.
 * Placeholders are kept verbatim; the UD service normalises them itself.</p>
 *
 * <h2>Command positions</h2>
 * A command's structural position is its <b>gap index</b>: the number of spoken tokens preceding it.
 * 0 is utterance-initial, {@link #getTokenCount()} is utterance-final. That index is stable under
 * re-wording in a way a character offset is not, which is what makes it usable as a learning label.
 *
 * <h2>Sentences, not utterances</h2>
 * The script grammar ends a {@code SceneUttr} at <em>any</em> punctuation mark, commas included:
 * {@code "Hallo $user, wie geht's Dir?"} parses as two utterances, {@code "Hallo $user,"} and
 * {@code "wie geht's Dir?"}. Handing a parser the first of those alone would be handing it a
 * fragment, and would defeat the UD service's own greeting guardrail, which exists precisely for
 * {@code "Hallo $user, …"}. {@link #sentencesOf(SceneTurn)} therefore merges consecutive utterances
 * until one ends in a sentence-final mark.
 *
 * <p>Splitting is still driven entirely by the parsed model, never by a regex over raw text — which
 * is what removes the old splitter's defect of cutting a command in half at a parameter's decimal
 * point ({@code intensity='0.8'}).</p>
 *
 * <p>Java 17 and Android-safe: JDK only.</p>
 *
 * @author Patrick Gebhard
 */
public final class UtteranceProjection {

    /** One spoken token — or the trailing punctuation mark — with both its clean and script spans. */
    public static final class Token {

        private final String mText;
        private final int mCleanFrom;
        private final int mCleanTo;
        private final int mScriptFrom;
        private final int mScriptTo;
        private final boolean mPunctuation;

        Token(final String text, final int cleanFrom, final int cleanTo,
              final int scriptFrom, final int scriptTo, final boolean punctuation) {
            mText = text;
            mCleanFrom = cleanFrom;
            mCleanTo = cleanTo;
            mScriptFrom = scriptFrom;
            mScriptTo = scriptTo;
            mPunctuation = punctuation;
        }

        public String getText() {
            return mText;
        }

        public int getCleanFrom() {
            return mCleanFrom;
        }

        public int getCleanTo() {
            return mCleanTo;
        }

        public int getScriptFrom() {
            return mScriptFrom;
        }

        public int getScriptTo() {
            return mScriptTo;
        }

        /** True for the synthetic token carrying the utterance's punctuation mark. */
        public boolean isPunctuation() {
            return mPunctuation;
        }

        @Override
        public String toString() {
            return "Token{'" + mText + "' clean=" + mCleanFrom + ".." + mCleanTo
                    + " script=" + mScriptFrom + ".." + mScriptTo + "}";
        }
    }

    /** Where one removed command sat, in both token and character terms. */
    public static final class CommandPosition {

        private final ActionObject mAction;
        private final int mTokenIndex;
        private final int mCleanOffset;

        CommandPosition(final ActionObject action, final int tokenIndex, final int cleanOffset) {
            mAction = action;
            mTokenIndex = tokenIndex;
            mCleanOffset = cleanOffset;
        }

        public ActionObject getAction() {
            return mAction;
        }

        /** Number of spoken tokens before the command: 0 = utterance-initial. */
        public int getTokenIndex() {
            return mTokenIndex;
        }

        /** Offset into {@link #getCleanText()} where the command sat — the boundary it occupied. */
        public int getCleanOffset() {
            return mCleanOffset;
        }

        /**
         * Script offset of the command's <b>actor-qualified name</b>: {@code "background"} for
         * {@code [background color='…']}, but {@code "time: init"} for {@code [time: init id='one']}.
         *
         * <p>Narrower than the bracket the author sees — {@link ActionObject} records only this span
         * in {@code lower}/{@code upper}, while {@link ActionObject#getText()} renders the whole
         * bracket, and each parameter's span lives on its own {@link ActionFeature}. Verified against
         * the charamel-embed example project. The load-bearing datum for placement is
         * {@link #getTokenIndex()} regardless of this.</p>
         */
        public int getScriptFrom() {
            return mAction.getLower();
        }

        /** Script offset just past the actor-qualified name — see {@link #getScriptFrom()}. */
        public int getScriptTo() {
            return mAction.getUpper();
        }

        public String getName() {
            return mAction.getName();
        }

        /** Declared actor, or {@code ""} for an unqualified command. */
        public String getActor() {
            final String actor = mAction.getActor();
            return actor == null ? "" : actor;
        }

        @Override
        public String toString() {
            return "CommandPosition{" + getActor() + " " + getName()
                    + " @token " + mTokenIndex + ", clean " + mCleanOffset + "}";
        }
    }

    private final String mCleanText;
    private final List<Token> mTokens;
    private final List<CommandPosition> mCommands;
    private final int mScriptFrom;
    private final int mScriptTo;

    private UtteranceProjection(
            final String cleanText,
            final List<Token> tokens,
            final List<CommandPosition> commands,
            final int scriptFrom,
            final int scriptTo) {
        mCleanText = cleanText;
        mTokens = Collections.unmodifiableList(tokens);
        mCommands = Collections.unmodifiableList(commands);
        mScriptFrom = scriptFrom;
        mScriptTo = scriptTo;
    }

    /** Marks that end a sentence. A comma, semicolon or colon continues it. */
    private static boolean isSentenceFinal(final String punct) {
        if (punct == null || punct.isEmpty()) {
            return false;
        }
        final char last = punct.charAt(punct.length() - 1);
        return last == '.' || last == '!' || last == '?' || last == '…';
    }

    /**
     * Groups a turn's utterances into sentences, merging consecutive utterances until one ends in a
     * sentence-final mark. The final group is always closed even if its punctuation is not terminal.
     * Empty projections are omitted.
     */
    public static List<UtteranceProjection> sentencesOf(final SceneTurn turn) {
        final List<UtteranceProjection> sentences = new ArrayList<>();
        if (turn == null || turn.getUttrList() == null) {
            return sentences;
        }
        final List<SceneUttr> pending = new ArrayList<>();
        for (final SceneUttr uttr : turn.getUttrList()) {
            pending.add(uttr);
            if (isSentenceFinal(uttr.getPunctuationMark())) {
                final UtteranceProjection projection = of(pending);
                if (!projection.isEmpty()) {
                    sentences.add(projection);
                }
                pending.clear();
            }
        }
        if (!pending.isEmpty()) {
            final UtteranceProjection projection = of(pending);
            if (!projection.isEmpty()) {
                sentences.add(projection);
            }
        }
        return sentences;
    }

    /**
     * Projects one utterance. Elements with empty text are skipped; an utterance consisting only of
     * commands yields an empty projection ({@link #isEmpty()}).
     */
    public static UtteranceProjection of(final SceneUttr uttr) {
        return of(uttr == null ? Collections.<SceneUttr>emptyList() : Collections.singletonList(uttr));
    }

    /**
     * Projects a run of consecutive utterances as one sentence. Each utterance contributes its
     * spoken tokens followed by its own punctuation mark, so an intermediate comma survives into the
     * clean text — the parser needs it to see the vocative.
     */
    public static UtteranceProjection of(final List<SceneUttr> utterances) {
        final List<Token> tokens = new ArrayList<>();
        final List<CommandPosition> commands = new ArrayList<>();
        final StringBuilder clean = new StringBuilder();
        int scriptFrom = -1;
        int scriptTo = 0;
        int spokenTokens = 0;

        for (final SceneUttr uttr : utterances) {
            if (uttr == null) {
                continue;
            }
            if (scriptFrom < 0) {
                scriptFrom = uttr.getLower();
            }
            scriptTo = Math.max(scriptTo, uttr.getUpper());

            if (uttr.getWordList() != null) {
                for (final UttrElement element : uttr.getWordList()) {
                    if (element instanceof ActionObject) {
                        commands.add(new CommandPosition(
                                (ActionObject) element, spokenTokens, clean.length()));
                        continue;
                    }
                    final String text = element.getText();
                    if (text == null || text.isEmpty()) {
                        continue;
                    }
                    if (clean.length() > 0) {
                        clean.append(' ');
                    }
                    final int cleanFrom = clean.length();
                    clean.append(text);
                    tokens.add(new Token(text, cleanFrom, clean.length(),
                            element.getLower(), element.getUpper(), false));
                    spokenTokens += 1;
                }
            }

            // The punctuation mark is a SceneUttr attribute, not an element, so it carries no offsets
            // of its own — it sits immediately after the last spoken token. Appending it matters: the
            // parser needs a terminated sentence to classify the clause type, and an intermediate
            // comma is what makes a vocative recognisable.
            final String punct = uttr.getPunctuationMark();
            if (punct != null && !punct.isEmpty()) {
                final int cleanFrom = clean.length();
                clean.append(punct);
                final int punctScriptFrom = tokens.isEmpty()
                        ? uttr.getLower()
                        : tokens.get(tokens.size() - 1).getScriptTo();
                tokens.add(new Token(punct, cleanFrom, clean.length(),
                        punctScriptFrom, punctScriptFrom + punct.length(), true));
            }
        }

        return new UtteranceProjection(
                clean.toString(), tokens, commands,
                scriptFrom < 0 ? 0 : scriptFrom, scriptTo);
    }

    /** The utterance as the parser should see it: spoken words plus punctuation, commands removed. */
    public String getCleanText() {
        return mCleanText;
    }

    /** Spoken tokens in order, with the punctuation mark last when present. */
    public List<Token> getTokens() {
        return mTokens;
    }

    /** Spoken tokens only, excluding the punctuation mark — the count gap indices run over. */
    public int getTokenCount() {
        int count = 0;
        for (final Token token : mTokens) {
            if (!token.isPunctuation()) {
                count += 1;
            }
        }
        return count;
    }

    /** The removed commands, in script order. */
    public List<CommandPosition> getCommands() {
        return mCommands;
    }

    /** True when the utterance contributes no spoken text (e.g. commands only). */
    public boolean isEmpty() {
        return mCleanText.isEmpty();
    }

    public int getScriptFrom() {
        return mScriptFrom;
    }

    public int getScriptTo() {
        return mScriptTo;
    }

    /**
     * Maps a span in {@link #getCleanText()} back to a span in the original script text.
     *
     * <p>The mapping is not affine — the projection joins tokens with a single space while the script
     * may separate them by arbitrary whitespace, newlines and command brackets — so it is resolved
     * through the token table. An offset falling in the gap between tokens snaps outward: a start
     * offset to the following token's beginning, an end offset to the preceding token's end, so a
     * mapped span never straddles material the parser never saw.</p>
     *
     * @return {@code {scriptFrom, scriptTo}}; for an empty projection, the utterance's own bounds
     */
    public int[] toScriptSpan(final int cleanFrom, final int cleanTo) {
        if (mTokens.isEmpty()) {
            return new int[]{mScriptFrom, mScriptTo};
        }
        final int from = toScriptOffset(cleanFrom, false);
        final int to = toScriptOffset(cleanTo, true);
        return new int[]{from, Math.max(from, to)};
    }

    /**
     * Maps a single clean-text offset to a script offset.
     *
     * @param asEnd treat the offset as the exclusive end of a span rather than its start; this
     *              decides which side of a token boundary the offset belongs to
     */
    public int toScriptOffset(final int cleanOffset, final boolean asEnd) {
        if (mTokens.isEmpty()) {
            return asEnd ? mScriptTo : mScriptFrom;
        }
        final int clamped = Math.max(0, Math.min(cleanOffset, mCleanText.length()));

        for (final Token token : mTokens) {
            final boolean inside = asEnd
                    ? (clamped > token.getCleanFrom() && clamped <= token.getCleanTo())
                    : (clamped >= token.getCleanFrom() && clamped < token.getCleanTo());
            if (inside) {
                final int offset = token.getScriptFrom() + (clamped - token.getCleanFrom());
                return Math.min(offset, token.getScriptTo());
            }
        }

        // In a gap, before the first token, or past the last one — snap outward.
        if (asEnd) {
            Token previous = mTokens.get(0);
            for (final Token token : mTokens) {
                if (token.getCleanTo() <= clamped) {
                    previous = token;
                }
            }
            return previous.getScriptTo();
        }
        for (final Token token : mTokens) {
            if (token.getCleanFrom() >= clamped) {
                return token.getScriptFrom();
            }
        }
        return mTokens.get(mTokens.size() - 1).getScriptTo();
    }

    /**
     * The gap index corresponding to a clean-text offset: the number of spoken tokens that end at or
     * before it.
     *
     * <p>This is what makes a semantic anchor slot comparable with an authored command's
     * {@link CommandPosition#getTokenIndex()}. The two come from different tokenisations — the parser
     * splits punctuation and multi-word tokens, the script model does not — so a character offset is
     * the only common coordinate, and this converts one into the other. Punctuation is not counted,
     * matching {@link #getTokenCount()}.</p>
     */
    public int tokenIndexAtCleanOffset(final int cleanOffset) {
        int index = 0;
        for (final Token token : mTokens) {
            if (token.isPunctuation()) {
                continue;
            }
            if (token.getCleanTo() <= cleanOffset) {
                index += 1;
            }
        }
        return index;
    }

    /**
     * Maps a script offset into {@link #getCleanText()}, or -1 when the offset falls inside a
     * command or otherwise outside the spoken text.
     */
    public int toCleanOffset(final int scriptOffset) {
        for (final Token token : mTokens) {
            if (scriptOffset >= token.getScriptFrom() && scriptOffset <= token.getScriptTo()) {
                return token.getCleanFrom() + (scriptOffset - token.getScriptFrom());
            }
        }
        return -1;
    }

    @Override
    public String toString() {
        return "UtteranceProjection{'" + mCleanText + "', " + mTokens.size() + " tokens, "
                + mCommands.size() + " commands}";
    }
}
