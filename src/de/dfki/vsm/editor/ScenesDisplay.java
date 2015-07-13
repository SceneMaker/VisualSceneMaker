//package de.dfki.vsm.editor;
//
////~--- non-JDK imports --------------------------------------------------------
//
//import de.dfki.vsm.editor.project.EditorProject;
//import de.dfki.vsm.editor.event.SceneSelectedEvent;
//import de.dfki.vsm.model.scenescript.SceneGroup;
//import de.dfki.vsm.model.scenescript.SceneScript;
//import de.dfki.vsm.util.evt.EventDispatcher;
//import de.dfki.vsm.util.log.LOGDefaultLogger;
//
//import static de.dfki.vsm.editor.util.Preferences.sHIGHLIGHT_COLOR;
//import static de.dfki.vsm.editor.util.Preferences.sWORKSPACEFONTSIZE;
//
////~--- JDK imports ------------------------------------------------------------
//
//import java.awt.BasicStroke;
//import java.awt.Color;
//import java.awt.Component;
//import java.awt.Container;
//import java.awt.Dimension;
//import java.awt.Font;
//import java.awt.FontMetrics;
//import java.awt.Graphics2D;
//import java.awt.LayoutManager2;
//import java.awt.RenderingHints;
//import java.awt.datatransfer.Transferable;
//import java.awt.dnd.DnDConstants;
//import java.awt.dnd.DragGestureEvent;
//import java.awt.dnd.DragGestureListener;
//import java.awt.dnd.DragSource;
//import java.awt.dnd.DragSourceDragEvent;
//import java.awt.dnd.DragSourceDropEvent;
//import java.awt.dnd.DragSourceEvent;
//import java.awt.dnd.DragSourceListener;
//import java.awt.event.MouseEvent;
//import java.awt.event.MouseListener;
//import java.awt.font.TextAttribute;
//
//import java.text.StringCharacterIterator;
//
//import java.util.ArrayList;
//import java.util.EventListener;
//import java.util.HashMap;
//import java.util.Hashtable;
//import java.util.Map;
//import java.util.Map.Entry;
//import java.util.Observable;
//import java.util.Observer;
//
//import javax.swing.BorderFactory;
//import javax.swing.Box;
//import javax.swing.BoxLayout;
//import javax.swing.JComponent;
//import javax.swing.JPanel;
//
///**
// * * @author Patrick Gebhard
// */
//public class ScenesDisplay extends JPanel implements Observer {
//    private final LOGDefaultLogger      mLogger         = LOGDefaultLogger.getInstance();
//    private final ArrayList<SceneBadge> mSceneBadgeList = new ArrayList<SceneBadge>();
//
//    public ScenesDisplay() {
//        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
//    }
//
//    /**
//     *
//     */
//    public void showScenes(EditorProject project) {
//        SceneScript sceneScript = project.getSceneScript();
//
//        if (sceneScript != null) {
//            HashMap<String, SceneGroup> sceneGroupMap = sceneScript.getSceneGroupMap();
//
//            // Show the scenes of the given project
//            for (Entry<String, SceneGroup> entry : sceneGroupMap.entrySet()) {
//                String     sceneGroupName = entry.getKey();
//                SceneGroup sceneGroup     = entry.getValue();
//
//                //
//                int sceneGroupSize = sceneGroup.getSize();
//
//                //
//                SceneBadge sceneBadge = new SceneBadge(sceneGroup, sceneGroupName, sceneGroupSize);
//
//                sceneBadge.setLayout(new SceneBadge.Layout(20));
//                mSceneBadgeList.add(sceneBadge);
//
//                // Make the panel for the scene badge
//                JPanel panel = new JPanel();
//
//                panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
//                panel.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 1));
//                panel.add(sceneBadge);
//                add(panel);
//            }
//
//            add(Box.createVerticalGlue());
//        }
//    }
//
//    @Override
//    public void update(Observable obs, java.lang.Object obj) {
//        System.err.println("Updating Scenes");
//
//        if (obj instanceof EditorProject) {
//            EditorProject project     = (EditorProject) obj;
//            SceneScript sceneScript = project.getSceneScript();
//
//            if (sceneScript != null) {
//
//                //
//                if (!sceneScript.isSceneListEmpty()) {
//                    removeAll();
//                    showScenes(project);
//                } else {
//                    removeAll();
//                    add(Box.createVerticalBox());
//                }
//
////              if (project.hasScenes()) {
////                  removeAll();
////                  showScenes(project);
////              } else {
////                  removeAll();
////                  add(Box.createVerticalBox());
////              }
//                revalidate();
//                repaint();
//            }
//        }
//    }
//
//    public static class SceneBadge extends JComponent implements EventListener, MouseListener, Transferable {
//        private FontMetrics mFM         = null;
//        private int         mTextIndent = 5;
//        private int         mSizeOffset = mTextIndent * 2;
//        public String       mSceneName;
//        private String      mVariants;
//        private SceneGroup  mSceneGroup;
//        public int          mHeight;
//
//        // Drag & Drop support
//        private DragSource          drag_source;
//        private DragGestureListener drag_gesture_listener;
//        private DragSourceListener  drag_source_listener;
//        private int                 acceptable_dnd_actions;
//
//        public SceneBadge(SceneGroup group, String name, int variants) {
//            mSceneName  = name;
//            mVariants   = " (" + variants + ")";
//            mSceneGroup = group;
//            setToolTipText(name);
//
//            // general font setup
//            Font mFont = new Font("SansSerif", Font.PLAIN, /* sBUILDING_BLOCK_FONT_SIZE */ sWORKSPACEFONTSIZE);
//
//            mFM = getFontMetrics(mFont);
//
//            Map<TextAttribute, Object> map = new Hashtable<TextAttribute, Object>();
//
//            map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON);
//            setFont(mFont.deriveFont(map));
//            mHeight = mFM.getHeight() + (mFM.getHeight() / 2);
//            addMouseListener(this);
//            initDnDSupport();
//        }
//
//        /**
//         * Init Drag & Drop support
//         */
//        private void initDnDSupport() {
//            final java.awt.datatransfer.Transferable transferable = this;
//
//            // Create the default drag source
//            drag_source = DragSource.getDefaultDragSource();
//
//            // Install the drag source listener
//            drag_source_listener = new DragSourceListener() {
//                public void dragEnter(DragSourceDragEvent dsde) {}
//                public void dragOver(DragSourceDragEvent dsde) {}
//                public void dropActionChanged(DragSourceDragEvent dsde) {}
//                public void dragExit(DragSourceEvent dse) {}
//                public void dragDropEnd(DragSourceDropEvent dsde) {}
//            };
//
//            // Install the drag gesture listener
//            drag_gesture_listener = new DragGestureListener() {
//                public void dragGestureRecognized(DragGestureEvent event) {
//
//                    ////System.out.println("Node.dragGestureRecognized");
//                    drag_source.startDrag(event, DragSource.DefaultCopyDrop, transferable, drag_source_listener);
//                }
//            };
//
//            // Set the acceptable actions
//            acceptable_dnd_actions = DnDConstants.ACTION_COPY;
//
//            // Set the default drag gesture recognizer
//            drag_source.createDefaultDragGestureRecognizer(this, acceptable_dnd_actions, drag_gesture_listener);
//        }
//
//        /* Drag&Drop support */
//        public Object getTransferData(java.awt.datatransfer.DataFlavor flavor)
//                throws java.awt.datatransfer.UnsupportedFlavorException, java.io.IOException {
//            return this;
//        }
//
//        public boolean isDataFlavorSupported(java.awt.datatransfer.DataFlavor flavor) {
//            return true;
//        }
//
//        public java.awt.datatransfer.DataFlavor[] getTransferDataFlavors() {
//            return null;
//
////          java.awt.datatransfer.DataFlavor[] flavours = new java.awt.datatransfer.DataFlavor[3];
////          flavours[0] = DataFlavor.stringFlavor;
////          flavours[1] = DataFlavor.imageFlavor;
////          try {
////          flavours[2] = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType);
////          }catch(ClassNotFoundException e) {
////              e.printStackTrace();
////          }
////          return flavours;
//        }
//
//        @Override
//        public void mouseClicked(MouseEvent e) {
//            EventDispatcher.getInstance().convey(new SceneSelectedEvent(this, mSceneGroup));
//        }
//
//        @Override
//        public void mouseEntered(MouseEvent e) {}
//
//        @Override
//        public void mouseExited(MouseEvent e) {}
//
//        @Override
//        public void mousePressed(MouseEvent e) {}
//
//        @Override
//        public void mouseReleased(MouseEvent e) {}
//
//        private String truncateName(String name, int size) {
//            if (mFM.stringWidth(name) > size) {
//                String                  placeHolder = "...";
//                StringBuilder           beginStr    = new StringBuilder();
//                StringBuilder           endStr      = new StringBuilder();
//                StringCharacterIterator bsi         = new StringCharacterIterator(name);
//                StringCharacterIterator esi         = new StringCharacterIterator(name);
//                char                    d           = esi.last();
//
//                for (char c = bsi.first(); c != StringCharacterIterator.DONE; c = bsi.next()) {
//                    if ((mFM.stringWidth(beginStr + placeHolder + endStr) + mFM.charWidth(c)) < size) {
//                        beginStr.append(c);
//                    } else {
//                        break;
//                    }
//
//                    if ((mFM.stringWidth(beginStr + placeHolder + endStr) + mFM.charWidth(d)) < size) {
//                        endStr.append(d);
//                        d = esi.previous();
//                    } else {
//                        break;
//                    }
//                }
//
//                return beginStr + placeHolder + endStr.reverse();
//            } else {
//                return name;
//            }
//        }
//
//        @Override
//        public void paintComponent(java.awt.Graphics g) {
//            Dimension  size     = getSize();
//            Graphics2D graphics = (Graphics2D) g;
//
//            // draw background
//            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
//            graphics.setColor(sHIGHLIGHT_COLOR);
//            graphics.fillRoundRect(0, 0, size.width, size.height, 5, 5);
//            graphics.setStroke(new BasicStroke(1.5f));
//
////          graphics.setColor(sHIGHLIGHTCOLOR.darker());
////          graphics.drawRoundRect(0, 0, size.width - 1, size.height - 1, 5, 5);
//            // draw the scene name
//            graphics.setStroke(new BasicStroke(1.5f));
//            graphics.setColor(Color.BLACK);
//            graphics.drawString(truncateName(mSceneName, size.width - (mTextIndent * 2) - mFM.stringWidth(mVariants))
//                                + mVariants, mTextIndent, mFM.getHeight());
//        }
//
//        /**
//         *
//         */
//        public static class Layout implements LayoutManager2 {
//            private int       mHeight    = 20;
//            private Dimension mMinimum   = new Dimension(100, mHeight);
//            private Dimension mPreferred = new Dimension(100, mHeight);
//
//            public Layout(int height) {
//                mHeight = height;
//            }
//
//            public void layoutContainer(Container parent) {
//                Dimension dim = parent.getSize();
//
//                dim.height = mHeight;
//            }
//
//            public Dimension minimumLayoutSize(Container parent) {
//                return mMinimum;
//            }
//
//            public Dimension preferredLayoutSize(Container parent) {
//                return mPreferred;
//            }
//
//            public Dimension maximumLayoutSize(Container target) {
//                return new Dimension(1000, mHeight);
//            }
//
//            public float getLayoutAlignmentX(Container target) {
//                return 0.5f;
//            }
//
//            public float getLayoutAlignmentY(Container target) {
//                return 0.5f;
//            }
//
//            public void invalidateLayout(Container target) {}
//
//            public void addLayoutComponent(Component comp, Object constraints) {}
//
//            public void addLayoutComponent(String name, Component comp) {}
//
//            public void removeLayoutComponent(Component comp) {}
//        }
//    }
//}
