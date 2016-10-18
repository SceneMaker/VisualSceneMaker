package de.dfki.vsm.util.syn;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.Toolkit;

import java.util.LinkedList;
import java.util.Map;

import javax.swing.event.DocumentEvent;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.PlainView;
import javax.swing.text.Segment;
import javax.swing.text.ViewFactory;

/**
 * @author Gregor Mehlmann
 */
public class SyntaxEditorView extends PlainView {
    private static RenderingHints sRendHints = null;

    static {
        try {
            Toolkit                                                   toolkit = Toolkit.getDefaultToolkit();
            @SuppressWarnings("unchecked") Map<RenderingHints.Key, ?> map     =
                (Map<RenderingHints.Key, ?>) toolkit.getDesktopProperty("awt.font.desktophints");

            //
            sRendHints = new RenderingHints(map);
        } catch (Throwable t) {}
    }

    // The Syntax Style Policy
    private final SyntaxStylePolicy mPolicy = new SyntaxStylePolicy(Preferences.sSTYLESURL);

    // The Singelton Logger
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public SyntaxEditorView(final Element element) {
        super(element);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    public final SyntaxStylePolicy getPolicy() {
        return mPolicy;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    private int drawDefaultText(final Graphics graphics, int x, int y, int p0, int p1) throws BadLocationException {

        // mLogger.message("Drawing Unselected Text From '" + p0 + "' Until '" + p1 + "'");
        // Get The Graphics Object
        final Graphics2D graphics2D = (Graphics2D) graphics;

        // Set The Rendering Hints
        graphics2D.addRenderingHints(sRendHints);

        //
        // graphics2D.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
        // Remember The Current Font
        final Font font = graphics2D.getFont();

        // Get The Syntax Document
        final SyntaxDocument document = (SyntaxDocument) getDocument();

        // Get The Current Symbol List
        final LinkedList<SyntaxDocSymbol> list = document.getSymbolList(p0, p1);

        //
        for (final SyntaxDocSymbol symbol : list) {
            final SyntaxDocToken token = symbol.getValue();

            // Get The Token Segment
            Segment segment = new Segment();

            segment.setPartialReturn(false);

            // Compute  Text Interval
            final int s = Math.max(p0, token.getLower());
            final int e = Math.min(p1, token.getUpper());

            // Compute The New Segment
            document.getText(s, e - s, segment);

            // Draw The Token Segment
            x = mPolicy.drawStyle(segment, x, y, graphics2D, this, s, token.getField());
        }

        // Reset Back To The Old Font
        graphics2D.setFont(font);

        // Return The Result Of The Call
        return x;
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected int drawUnselectedText(final Graphics graphics, int x, int y, int p0, int p1)
            throws BadLocationException {
        return drawDefaultText(graphics, x, y, p0, p1);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected int drawSelectedText(Graphics graphics, int x, int y, int p0, int p1) throws BadLocationException {
        return drawDefaultText(graphics, x, y, p0, p1);
    }

    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    @Override
    protected void updateDamage(final DocumentEvent changes, final Shape shape, final ViewFactory factory) {
        super.updateDamage(changes, shape, factory);
        getContainer().repaint();
    }
}
