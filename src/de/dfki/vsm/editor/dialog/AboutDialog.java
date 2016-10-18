package de.dfki.vsm.editor.dialog;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.OKButton;
import de.dfki.vsm.editor.util.SImageView;

import static de.dfki.vsm.editor.dialog.Dialog.getFillerBox;
import static de.dfki.vsm.Preferences.sABOUT_FILE;

//~--- JDK imports ------------------------------------------------------------

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import java.io.IOException;

import java.net.URL;

import java.util.Timer;
import java.util.TimerTask;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSeparator;
import javax.swing.JViewport;
import javax.swing.border.EmptyBorder;
import javax.swing.text.Element;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;

/**
 * @author Patrick Gebhard
 * @author Gregor Mehlmann
 */
public class AboutDialog extends JDialog {

    // Singelton instance
    private static AboutDialog sInstance            = null;
    private Font               mFont                = new Font("SansSerif", Font.PLAIN, 11);
    private JPanel             mContentPanel        = null;
    private JScrollPane        mAboutTextScrollPane = null;
    private MyEditorPane       mAboutPane           = null;
    private JViewport          mViewPort            = null;
    private Timer              mScrollTimer         = null;
    
    protected HTMLEditorKit    editorKit            = new HTMLEditorKit() {
        @Override
        public ViewFactory getViewFactory() {
            return new HTMLEditorKit.HTMLFactory() {
                @Override
                public View create(Element elem) {
                    Object o = elem.getAttributes().getAttribute(StyleConstants.NameAttribute);

                    if (o instanceof HTML.Tag) {
                        HTML.Tag kind = (HTML.Tag) o;

                        if (kind == HTML.Tag.IMG) {

                            // bypass problems with relative image filenames and documents assigned via setText()
                            return new SImageView(elem);
                        }
                    }

                    return super.create(elem);
                }
            };
        }
    };
    private OKButton mOkButton;

    // Construction
    private AboutDialog() {
        super(EditorInstance.getInstance(), "About", false);
        EditorInstance.getInstance().addEscapeListener(this);
        // Init close operation
        setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent we) {
                mScrollTimer.cancel();
                dispose();
            }
        });

        // the look and feel ...
        JPanel logoPanel = new JPanel();

        logoPanel.setLayout(new BoxLayout(logoPanel, BoxLayout.X_AXIS));
        logoPanel.add(new JLabel(Preferences.ICON_SCENEMAKER_LOGO));

        int logoXSize = Preferences.ICON_SCENEMAKER_LOGO.getIconWidth();

        mAboutPane = new MyEditorPane();
        mAboutPane.setEditorKit(editorKit);
        mAboutPane.setContentType("text/html");
        mAboutPane.setEditable(false);
        mAboutPane.setHighlighter(null);
        mAboutPane.setDragEnabled(false);
        mAboutPane.setBackground(new Color(224, 223, 227));

        try {
            URL pageURL = sABOUT_FILE;           
            mAboutPane.setPage(pageURL);
        } catch (Exception e) {
            mAboutPane.setText("<html><body><font color=\"red\">No about available!<br>Unable to locate " + sABOUT_FILE
                               + "</font></body></html>");
            e.printStackTrace();
        }

        // Init the scroll pane
        mAboutTextScrollPane = new JScrollPane(mAboutPane);
        mAboutTextScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
        mAboutTextScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        mAboutTextScrollPane.setViewportBorder(new EmptyBorder(0, 0, 0, 0));
        mAboutTextScrollPane.setBorder(new EmptyBorder(0, 0, 0, 0));

        // Init the button
        // Ok button
        mOkButton = new OKButton();
        mOkButton.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                setVisible(false);
                mScrollTimer.cancel();
                dispose();
            }
        });
        setFont(mFont);

        JPanel buttonPanel = new JPanel();

        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
        buttonPanel.add(getFillerBox(100, 10, 2000, 10));
        buttonPanel.add(mOkButton);

        JSeparator separator = new JSeparator();

        separator.setMinimumSize(new Dimension(logoXSize, 2));
        separator.setPreferredSize(new Dimension(logoXSize, 2));
        separator.setMaximumSize(new Dimension(logoXSize, 2));
        mContentPanel = new JPanel();
        mContentPanel.setLayout(new BoxLayout(mContentPanel, BoxLayout.Y_AXIS));
        mContentPanel.add(logoPanel);
        mContentPanel.add(getFillerBox(logoXSize + 6, 2, logoXSize + 6, 2));
        mContentPanel.add(mAboutTextScrollPane);
        mContentPanel.add(getFillerBox(logoXSize + 6, 2, logoXSize + 6, 2));
        mContentPanel.add(buttonPanel);
        getContentPane().add(mContentPanel, BorderLayout.CENTER);
        setSize(new Dimension(logoXSize + 6, 300));

        // Start auto scroller
        mScrollTimer = new Timer(true);
        mScrollTimer.schedule(new ScrollTask(), 2000, 80);

        Dimension bounds  = Toolkit.getDefaultToolkit().getScreenSize();
        Dimension abounds = getSize();

        setLocation((bounds.width - abounds.width) / 2, (bounds.height - abounds.height) / 3);
        setResizable(false);
        setVisible(true);
    }

    // Get the singelton instance
    public static AboutDialog getInstance() {
        if (sInstance == null) {
            sInstance = new AboutDialog();
        }

        return sInstance;
    }

    class MyEditorPane extends JEditorPane {
        public MyEditorPane() {
            super();
        }

        public MyEditorPane(String strURL) throws IOException {
            super(strURL);
        }

        public MyEditorPane(URL oInitialPage) throws IOException {
            super(oInitialPage);
        }

        public MyEditorPane(String strType, String strText) {
            super(strType, strText);
        }

        @Override
        protected void paintComponent(Graphics g) {
            Graphics2D g2 = (Graphics2D) g;

            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            super.paintComponent(g2);
        }
    }


    class ScrollTask extends TimerTask {
        int       xPos        = 0;
        int       yPos        = 0;
        int       initialYPos = 0;
        int       height      = 0;
        JViewport fViewPort   = null;
        boolean   configured  = false;

        public ScrollTask() {}

        public void run() {
            if (!configured) {
                fViewPort = mAboutTextScrollPane.getViewport();

                // fViewPort.setScrollMode(JViewport.BLIT_SCROLL_MODE);
                Rectangle viewRect = fViewPort.getViewRect();

                xPos        = new Double(viewRect.getX()).intValue();
                yPos        = new Double(viewRect.getY()).intValue();
                initialYPos = yPos;
                height      = mAboutPane.getSize().height;
                configured  = true;
            } else {
                yPos = (yPos <= height)
                       ? yPos + 1
                       : initialYPos;
                fViewPort.setViewPosition(new Point(xPos, yPos));
            }
        }
    }
}
