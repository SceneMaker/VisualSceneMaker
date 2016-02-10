package de.dfki.vsm.editor.project.sceneflow;

//~--- non-JDK imports --------------------------------------------------------
import com.sun.java.swing.plaf.windows.WindowsScrollBarUI;
import de.dfki.vsm.editor.event.NodeExecutedEvent;
import de.dfki.vsm.editor.project.EditorProject;
import de.dfki.vsm.editor.project.sceneflow.attributes.ElementEditor;
import de.dfki.vsm.editor.project.sceneflow.elements.SceneFlowElementPanel;
import de.dfki.vsm.editor.project.sceneflow.elements.SceneFlowPalettePanel;
import de.dfki.vsm.editor.project.sceneflow.workspace.WorkSpacePanel;
import de.dfki.vsm.Preferences;
import de.dfki.vsm.editor.EditorInstance;
import de.dfki.vsm.editor.event.ElementEditorToggledEvent;
import de.dfki.vsm.editor.event.NodeSelectedEvent;
import de.dfki.vsm.editor.project.ProjectEditor;
import de.dfki.vsm.editor.util.SceneFlowManager;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.evt.EventListener;
import de.dfki.vsm.util.evt.EventObject;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import java.io.IOException;

import java.util.Timer;
import java.util.TimerTask;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.TransferHandler;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import javax.swing.undo.UndoManager;

/**
 * @author Gregor Mehlmann
 * @author Patrick Gebhard
 */
public final class SceneFlowEditor extends JPanel implements EventListener
{

    // The singelton logger instance
    private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();

    // TODO: move undo manager up at least to project editor
    private UndoManager mUndoManager = null;

    // TODO: make final singelton timer
    private Timer mVisualizationTimer = new Timer("SceneFlowEditor-Timer");

    //
    private final SceneFlow mSceneFlow;
    private final EditorProject mEditorProject;

    // TODO: remove sceneflow manager
    private final SceneFlowManager mSceneFlowManager;

    // The GUI components of the editor
    private final WorkSpacePanel mWorkSpacePanel;
    private final SceneFlowToolBar mSceneFlowToolBar;
    private final ElementEditor mElementEditor;
    private final SceneFlowPalettePanel mStaticElementsPanel;
    private final SceneFlowElementPanel mDynamicElementsPanel;
    private final JPanel mNewElementDisplay;
    private final JLabel mFooterLabel;
    private final JSplitPane mSplitPane;
    private final JScrollPane mWorkSpaceScrollPane;
    private final EventDispatcher mEventCaster = EventDispatcher.getInstance();

    // Create a sceneflow editor
    public SceneFlowEditor(final EditorProject project)
    {

        // Initialize the editor project
        mEditorProject = project;

        // Initialize the sceneflow
        mSceneFlow = mEditorProject.getSceneFlow();

        final Polygon pUp = new Polygon();

        pUp.addPoint(1, 4);
        pUp.addPoint(5, 0);
        pUp.addPoint(9, 4);

        final Polygon pDown = new Polygon();

        pDown.addPoint(13, 0);
        pDown.addPoint(17, 4);
        pDown.addPoint(21, 0);
        mSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        mSplitPane.setBorder(BorderFactory.createEmptyBorder());
        mSplitPane.setContinuousLayout(true);
        mSplitPane.setUI(new BasicSplitPaneUI()
        {
            @Override
            public BasicSplitPaneDivider createDefaultDivider()
            {
                return new BasicSplitPaneDivider(this)
                {
                    @Override
                    public void setBorder(Border b)
                    {
                    }

                    @Override
                    public void paint(Graphics g)
                    {
                        Graphics2D graphics = (Graphics2D) g;

                        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                        Rectangle r = getBounds();

                        graphics.setColor(UIManager.getColor("Panel.background"));
                        graphics.fillRect(0, 0, r.width - 1, r.height);
//
//                        // graphics.setColor(new Color(100, 100, 100));
                        graphics.fillRect((r.width / 2 - 25), 0, 50, r.height);
//                        graphics.drawPolygon(pUp);
//                        graphics.fillPolygon(pUp);
//                        graphics.drawPolygon(pDown);
//                        graphics.fillPolygon(pDown);
                    }

                    @Override
                    protected void processMouseEvent(MouseEvent me)
                    {
                        super.processMouseEvent(me);
                        switch (me.getID())
                        {
                            case MouseEvent.MOUSE_CLICKED:
                                toggleElementEditor();
                                ElementEditorToggledEvent ev = new ElementEditorToggledEvent(this);
                                mEventCaster.convey(ev);
                        }
                    }
                };
            }
        });
        mUndoManager = new UndoManager();
        mSceneFlowManager = new SceneFlowManager(mSceneFlow);

        // The center component is the workspace
        mWorkSpacePanel = new WorkSpacePanel(this, mEditorProject);
        mWorkSpacePanel.setTransferHandler(new SceneFlowImage());

        mWorkSpaceScrollPane = new JScrollPane(mWorkSpacePanel);
        mWorkSpaceScrollPane.getVerticalScrollBar().setUI(new WindowsScrollBarUI());
        mWorkSpaceScrollPane.getHorizontalScrollBar().setUI(new WindowsScrollBarUI());
        mWorkSpaceScrollPane.setBorder(BorderFactory.createEtchedBorder());

        // The west component is the workbar
        mFooterLabel = new JLabel();
        mDynamicElementsPanel = new SceneFlowElementPanel(mEditorProject);
        mStaticElementsPanel = new SceneFlowPalettePanel();
        mElementEditor = new ElementEditor();
        mSceneFlowToolBar = new SceneFlowToolBar(this, mEditorProject);

        // TODO: adding not explicit but via refresh method
        mSceneFlowToolBar.addPathComponent(mSceneFlow);

        //
        setLayout(new BorderLayout());
        add(mSceneFlowToolBar, BorderLayout.NORTH);
        mNewElementDisplay = new JPanel();
        mNewElementDisplay.setLayout(new BoxLayout(mNewElementDisplay, BoxLayout.Y_AXIS));
        mNewElementDisplay.add(mStaticElementsPanel);

        // mNewElementDisplay.add(new JSeparator(JSeparator.HORIZONTAL));
        mNewElementDisplay.add(mDynamicElementsPanel);

        // PG 17.12.13 - FUTURE FEATURE! mNewElementDisplay.add(new EdgeTypeSelection(), BorderLayout.NORTH);
        add(mNewElementDisplay, BorderLayout.WEST);
        mNewElementDisplay.setVisible(Boolean.valueOf(Preferences.getProperty("showelements"))
                ? true
                : false);
        mElementEditor.setVisible(Boolean.valueOf(Preferences.getProperty("showelementproperties"))
                ? true
                : false);

        //INITIALIZE THE SPLIT PANEL WITH WORK SPACE AND ELEMENTEDITOR
        mWorkSpaceScrollPane.setMinimumSize(new Dimension(10, 10));
        mWorkSpaceScrollPane.setMaximumSize(new Dimension(10000, 3000));
        mSplitPane.setLeftComponent(mWorkSpaceScrollPane);
        mElementEditor.setMinimumSize(new Dimension(260, 500));
        mElementEditor.setMaximumSize(new Dimension(10000, 3000));
        mSplitPane.setResizeWeight(1.0);
        mSplitPane.setRightComponent(mElementEditor);
        if (Boolean.valueOf(Preferences.getProperty("showelementproperties")))
        {

            mSplitPane.setDividerLocation((int) (EditorInstance.getInstance().getWidth() - 520));

            //mSplitPane.setDividerLocation(
            //    Integer.parseInt(mEditorProject.getEditorConfig().getProperty("propertiesdividerlocation")));
            //THIS EVENT IS CASTED ONLY TO ACTIVATE THE ELEMENT EDITOR WITH THE INFO OF THE CURRENT PROJECT
        }
        else
        {
            mSplitPane.setDividerLocation(1.0d);
        }
        add(mSplitPane, BorderLayout.CENTER);

        mSplitPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, new PropertyChangeListener()
        {
            @Override
            public void propertyChange(PropertyChangeEvent pce)
            {

                // solve issue here
                if (Preferences.getProperty("showelementproperties").equals("true"))
                {
                    mEditorProject.getEditorConfig().setProperty("propertiesdividerlocation", "" + mSplitPane.getDividerLocation());
                }
            }
        });

        //ACTIVATE THE CONTENT OF THE ElementEditor
        NodeSelectedEvent e = new NodeSelectedEvent(this, getSceneFlowManager().getCurrentActiveSuperNode());
        mEventCaster.convey(e);

        //
        mFooterLabel.setForeground(Color.red);
        add(mFooterLabel, BorderLayout.SOUTH);

    }

    // Update the visualization
    @Override
    public final void update(final EventObject event)
    {
        if (mVisualizationTimer != null)
        {
            if (event instanceof NodeExecutedEvent)
            {
                setMessageLabelText("SceneFlow is running", false);
            }
        }
    }

    public void setViewPosition(Point p)
    {
        mWorkSpaceScrollPane.getViewport().setViewPosition(p);
    }

    /**
     *
     *
     *
     *
     *
     */
    public void toggleElementEditor()
    {
        if (Boolean.valueOf(Preferences.getProperty("showelementproperties")))
        {
            mElementEditor.setVisible(false);
            Preferences.setProperty("showelementproperties", "false");
            Preferences.save();
            mSplitPane.setDividerLocation(1d);
        }
        else
        {
            mElementEditor.setVisible(true);
            Preferences.setProperty("showelementproperties", "true");
            Preferences.save();
            mSplitPane.setDividerLocation(
                    Integer.parseInt(mEditorProject.getEditorConfig().getProperty("propertiesdividerlocation")));
        }

    }

    public void expandTree()
    {
        mDynamicElementsPanel.expandTree();
    }

    public boolean isElementEditorVisible()
    {
        return mElementEditor.isVisible();
    }

    public void showElementDisplay()
    {
        if (mNewElementDisplay.isVisible())
        {
            mNewElementDisplay.setVisible(false);
            Preferences.setProperty("showelements", "false");
            Preferences.save();
        }
        else
        {
            mNewElementDisplay.setVisible(true);
            Preferences.setProperty("showelements", "true");
            Preferences.save();
            Preferences.save();
        }
    }

    public boolean isElementDisplayVisible()
    {
        return mNewElementDisplay.isVisible();
    }

    public final SceneFlowToolBar getToolBar()
    {
        return mSceneFlowToolBar;
    }

    public SceneFlowManager getSceneFlowManager()
    {
        return mSceneFlowManager;
    }

    public SceneFlow getSceneFlow()
    {
        return mSceneFlow;
    }

    public WorkSpacePanel getWorkSpace()
    {
        return mWorkSpacePanel;
    }

    public UndoManager getUndoManager()
    {
        return mUndoManager;
    }

    // TODO: adding not explicit but via refresh method
    public void addPathComponent(SuperNode supernode)
    {

        // TODO: adding not explicit but via refresh method
        mSceneFlowToolBar.addPathComponent(supernode);
    }

    public SuperNode removePathComponent()
    {
        return mSceneFlowToolBar.removePathComponent();
    }

    public void setMessageLabelText(String value)
    {
        setMessageLabelText(value, false);
    }

    public void setMessageLabelText(String value, boolean forever)
    {
        if (forever)
        {
            mFooterLabel.setText(value);
        }
        else
        {
            mFooterLabel.setText(value);

            VisuTask visuTask = new VisuTask(25, mFooterLabel);

            try
            {
                mVisualizationTimer.cancel();
                mVisualizationTimer = new Timer();
                mVisualizationTimer.schedule(visuTask, 0, 100);
            } catch (Exception e)
            {

                // e.printStackTrace();
            }
        }
    }

    public void close()
    {

        // Delete all observers
//      mObservable.deleteObservers();
        // Cleanup worspace
        mWorkSpacePanel.cleanup();

        // Stop visualization
        stopVisualisation();
    }

    /**
     * Nullifies the VisalisationTimer thread
     */
    public void stopVisualisation()
    {
        if (mVisualizationTimer != null)
        {
            mVisualizationTimer.purge();
            mVisualizationTimer.cancel();

            // TODO: Why setting to null
            // mVisuTimer = null;
        }
    }

    public JSplitPane getSplitPane()
    {
        return mSplitPane;
    }

    public JLabel getFooterLabel()
    {
        return mFooterLabel;
    }

//
//  /**
//   *
//   *
//   *
//   *
//   *
//   */
//  private class Observable extends java.util.Observable {
//
//      public void update(Object obj) {
//          setChanged();
//          notifyObservers(obj);
//      }
//  }
    public final void refresh()
    {

        // Print some information
        //mLogger.message("Refreshing '" + this + "'");
        // Refresh editor toolbar
        mSceneFlowToolBar.refresh();
        mStaticElementsPanel.refresh();
        mDynamicElementsPanel.refresh();
        mWorkSpacePanel.refresh();
        mElementEditor.refresh();
    }

    class SceneFlowImage extends TransferHandler implements Transferable
    {

        private final DataFlavor flavors[] =
        {
            DataFlavor.imageFlavor
        };
        private JPanel source;
        private Image image;

        @Override
        public int getSourceActions(JComponent c)
        {
            return TransferHandler.COPY;
        }

        @Override
        public boolean canImport(JComponent comp, DataFlavor flavor[])
        {
            if (!(comp instanceof JPanel))
            {
                return false;
            }

            for (int i = 0, n = flavor.length; i < n; i++)
            {
                for (int j = 0, m = flavors.length; j < m; j++)
                {
                    if (flavor[i].equals(flavors[j]))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        @Override
        public Transferable createTransferable(JComponent comp)
        {

            // Clear
            source = null;
            image = new BufferedImage(comp.getWidth(), comp.getHeight(), BufferedImage.TYPE_INT_RGB);

            if (comp instanceof JPanel)
            {
                JPanel panel = (JPanel) comp;
                Graphics g = image.getGraphics();

                comp.paint(g);
                g.dispose();
                source = panel;

                return this;
            }

            return null;
        }

        @Override
        public boolean importData(JComponent comp, Transferable t)
        {
            if (comp instanceof JPanel)
            {
                JPanel panel = (JPanel) comp;

                if (t.isDataFlavorSupported(flavors[0]))
                {
                    try
                    {
                        image = (Image) t.getTransferData(flavors[0]);
                        panel.paint(image.getGraphics());

                        return true;
                    } catch (UnsupportedFlavorException ignored)
                    {
                    } catch (IOException ignored)
                    {
                    }
                }
            }

            return false;
        }

        // Transferable
        public Object getTransferData(DataFlavor flavor)
        {
            if (isDataFlavorSupported(flavor))
            {
                return image;
            }

            return null;
        }

        public DataFlavor[] getTransferDataFlavors()
        {
            return flavors;
        }

        public boolean isDataFlavorSupported(DataFlavor flavor)
        {
            return flavor.equals(DataFlavor.imageFlavor);
        }
    }

    /**
     *
     *
     *
     *
     *
     */
    private class VisuTask extends TimerTask
    {

        int mSteps = 0;
        JLabel mLabel = null;
        Color mLabelColor = null;
        int mOpacyStep = 0;
        int mRedVal = 0;
        int mGreenVal = 0;
        int mBlueVal = 0;
        int mOpacyVal = 255;

        VisuTask(int steps, JLabel l)
        {
            mSteps = steps;
            mLabel = l;
            mLabelColor = l.getForeground();
            mRedVal = mLabelColor.getRed();
            mGreenVal = mLabelColor.getGreen();
            mBlueVal = mLabelColor.getBlue();
            mOpacyStep = 255 / mSteps;
        }

        public synchronized int getActivityTime()
        {
            return mSteps;
        }

        public void run()
        {
            mSteps = (mSteps > 0)
                    ? mSteps - 1
                    : 0;
            mOpacyVal -= mOpacyStep;
            mLabel.setForeground(new Color(mRedVal, mGreenVal, mBlueVal, mOpacyVal));
            mLabel.repaint();

            if (mSteps == 0)
            {
                mLabel.setText("");
                cancel();
            }
        }
    }
}
