package de.dfki.vsm.editor.util.autocompletation;

import org.fife.ui.autocomplete.AbstractCompletionProvider;
import org.fife.ui.autocomplete.Completion;
import org.fife.ui.autocomplete.CompletionProvider;
import org.fife.ui.autocomplete.ParameterizedCompletion;

import javax.swing.text.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;

public class PluginCompletionProvider extends AbstractCompletionProvider {
    protected Segment seg;
    private String lastCompletionsAtText;
    private List<Completion> lastParameterizedCompletionsAt;


    public PluginCompletionProvider(String characterName) {
        seg = new Segment();
        this.completions = new ArrayList();
    }

    @Override
    public String getAlreadyEnteredText(JTextComponent jTextComponent) {
        Document doc = jTextComponent.getDocument();

        int dot = jTextComponent.getCaretPosition();
        Element root = doc.getDefaultRootElement();
        int index = root.getElementIndex(dot);
        Element elem = root.getElement(index);
        int start = elem.getStartOffset();
        int len = dot-start;
        try {
            doc.getText(start, len, seg);
        } catch (BadLocationException ble) {
            ble.printStackTrace();
            return EMPTY_STRING;
        }

        int segEnd = seg.offset + len;
        start = segEnd - 1;
        while (start>=seg.offset && isValidChar(seg.array[start])) {
            start--;
        }
        start++;

        len = segEnd - start;
        return len==0 ? EMPTY_STRING : new String(seg.array, start, len);
    }

    private boolean isValidChar(char ch) {
        return Character.isLetterOrDigit(ch) || ch=='_';
    }

    @Override
    public List getCompletionsAt(JTextComponent tc, Point p) {
        int offset = tc.viewToModel(p);
        if (offset<0 || offset>=tc.getDocument().getLength()) {
            lastCompletionsAtText = null;
            return lastParameterizedCompletionsAt = null;
        }

        Segment s = new Segment();
        Document doc = tc.getDocument();
        Element root = doc.getDefaultRootElement();
        int line = root.getElementIndex(offset);
        Element elem = root.getElement(line);
        int start = elem.getStartOffset();
        int end = elem.getEndOffset() - 1;

        try {

            doc.getText(start, end-start, s);

            // Get the valid chars before the specified offset.
            int startOffs = s.offset + (offset-start) - 1;
            while (startOffs>=s.offset && isValidChar(s.array[startOffs])) {
                startOffs--;
            }

            // Get the valid chars at and after the specified offset.
            int endOffs = s.offset + (offset-start);
            while (endOffs<s.offset+s.count && isValidChar(s.array[endOffs])) {
                endOffs++;
            }

            int len = endOffs - startOffs - 1;
            if (len<=0) {
                return lastParameterizedCompletionsAt = null;
            }
            String text = new String(s.array, startOffs+1, len);

            if (text.equals(lastCompletionsAtText)) {
                return lastParameterizedCompletionsAt;
            }

            // Get a list of all Completions matching the text.
            List<Completion> list = getCompletionByInputText(text);
            lastCompletionsAtText = text;
            return lastParameterizedCompletionsAt = list;

        } catch (BadLocationException ble) {
            ble.printStackTrace(); // Never happens
        }

        lastCompletionsAtText = null;
        return lastParameterizedCompletionsAt = null;
    }

    @Override
    public List getParameterizedCompletions(JTextComponent tc) {
        List<ParameterizedCompletion> list = null;

        // If this provider doesn't support parameterized completions,
        // bail out now.
        char paramListStart = getParameterListStart();
        if (paramListStart==0) {
            return list; // null
        }

        int dot = tc.getCaretPosition();
        Segment s = new Segment();
        Document doc = tc.getDocument();
        Element root = doc.getDefaultRootElement();
        int line = root.getElementIndex(dot);
        Element elem = root.getElement(line);
        int offs = elem.getStartOffset();
        int len = dot - offs - 1/*paramListStart.length()*/;
        if (len<=0) { // Not enough chars on line for a method.
            return list; // null
        }

        try {

            doc.getText(offs, len, s);

            // Get the identifier preceding the '(', ignoring any whitespace
            // between them.
            offs = s.offset + len - 1;
            while (offs>=s.offset && Character.isWhitespace(s.array[offs])) {
                offs--;
            }
            int end = offs;
            while (offs>=s.offset && isValidChar(s.array[offs])) {
                offs--;
            }

            String text = new String(s.array, offs+1, end-offs);

            // Get a list of all Completions matching the text, but then
            // narrow it down to just the ParameterizedCompletions.
            List<Completion> l = getCompletionByInputText(text);
            if (l!=null && !l.isEmpty()) {
                for (int i=0; i<l.size(); i++) {
                    Object o = l.get(i);
                    if (o instanceof ParameterizedCompletion) {
                        if (list==null) {
                            list = new ArrayList<ParameterizedCompletion>(1);
                        }
                        list.add((ParameterizedCompletion)o);
                    }
                }
            }

        } catch (BadLocationException ble) {
            ble.printStackTrace(); // Never happens
        }

        return list;
    }
}
