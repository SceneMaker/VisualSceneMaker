package de.dfki.vsm.model.scenescript;

import de.dfki.vsm.util.syn.SyntaxDocSymbol;
import de.dfki.vsm.util.syn.SyntaxDocToken;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java_cup.runtime.Symbol;

public final class ScriptDiagnostics {

    public static final class Diagnostic {
        private final int mFrom;
        private final int mTo;
        private final int mLine;
        private final int mColumn;
        private final String mSeverity;
        private final String mMessage;
        private final String mSource;

        public Diagnostic(int from, int to, int line, int column, String severity, String message, String source) {
            mFrom = from;
            mTo = to;
            mLine = line;
            mColumn = column;
            mSeverity = severity;
            mMessage = message;
            mSource = source;
        }

        public int getFrom() {
            return mFrom;
        }

        public int getTo() {
            return mTo;
        }

        public int getLine() {
            return mLine;
        }

        public int getColumn() {
            return mColumn;
        }

        public String getSeverity() {
            return mSeverity;
        }

        public String getMessage() {
            return mMessage;
        }

        public String getSource() {
            return mSource;
        }
    }

    public static final class Result {
        private final boolean mParseOk;
        private final List<Diagnostic> mDiagnostics;

        public Result(boolean parseOk, List<Diagnostic> diagnostics) {
            mParseOk = parseOk;
            mDiagnostics = diagnostics == null ? Collections.emptyList() : diagnostics;
        }

        public boolean isParseOk() {
            return mParseOk;
        }

        public List<Diagnostic> getDiagnostics() {
            return mDiagnostics;
        }
    }

    public static Result analyze(String text) {
        String input = text == null ? "" : text;
        List<Diagnostic> diagnostics = new ArrayList<>();

        diagnostics.addAll(scanLexErrors(input));
        diagnostics.addAll(scanParseErrors(input));

        boolean parseOk = diagnostics.isEmpty();
        return new Result(parseOk, diagnostics);
    }

    private static List<Diagnostic> scanLexErrors(String text) {
        List<Diagnostic> diagnostics = new ArrayList<>();
        String sanitized = ScriptStructureScanner.scan(text).sanitizedText;
        ScriptLexxer lexxer = new ScriptLexxer(new StringReader(sanitized), true, false, false);
        try {
            SyntaxDocSymbol symbol;
            while ((symbol = (SyntaxDocSymbol) lexxer.next_token()) != null) {
                SyntaxDocToken token = symbol.getValue();
                if (token != null && "ERRORSTATE".equals(token.getField())) {
                    diagnostics.add(fromToken(token, "error", "Lexical error", "lexer"));
                }
            }
        } catch (Exception exc) {
            diagnostics.add(new Diagnostic(0, 0, 0, 0, "error", "Lexer failure: " + exc.getMessage(), "lexer"));
        }
        return diagnostics;
    }

    private static List<Diagnostic> scanParseErrors(String text) {
        String sanitized = ScriptStructureScanner.scan(text).sanitizedText;
        String normalized = ScriptParser.preprocessInput(sanitized);
        ScriptLexxer lexxer = new ScriptLexxer(new StringReader(normalized), true, false, false);
        ParserWithErrors parser = new ParserWithErrors(lexxer);
        try {
            parser.parse();
        } catch (Exception exc) {
            parser.addGenericError("Parser failure: " + exc.getMessage());
        }
        return parser.getErrors();
    }

    private static Diagnostic fromToken(SyntaxDocToken token, String severity, String message, String source) {
        int from = token.getLower();
        int to = Math.max(token.getUpper(), from + 1);
        int line = token.getLine() + 1;
        int column = token.getColumn() + 1;
        String detail = token.getValue();
        String msg = (detail == null || detail.isBlank()) ? message : message + " near '" + detail + "'";
        return new Diagnostic(from, to, line, column, severity, msg, source);
    }

    private static final class ParserWithErrors extends ScriptParser {
        private final List<Diagnostic> mErrors = new ArrayList<>();

        private ParserWithErrors(ScriptLexxer lexxer) {
            super(lexxer, false);
        }

        @Override
        public void syntax_error(Symbol symbol) {
            recordError(symbol, "Syntax error");
            super.done_parsing();
        }

        @Override
        public void unrecovered_syntax_error(Symbol symbol) throws Exception {
            recordError(symbol, "Unrecovered syntax error");
            super.done_parsing();
        }

        private void recordError(Symbol symbol, String message) {
            if (symbol != null && symbol.value instanceof SyntaxDocToken) {
                SyntaxDocToken token = (SyntaxDocToken) symbol.value;
                mErrors.add(fromToken(token, "error", message, "parser"));
            } else {
                mErrors.add(new Diagnostic(0, 0, 0, 0, "error", message, "parser"));
            }
        }

        private void addGenericError(String message) {
            mErrors.add(new Diagnostic(0, 0, 0, 0, "error", message, "parser"));
        }

        private List<Diagnostic> getErrors() {
            return mErrors;
        }
    }
}
