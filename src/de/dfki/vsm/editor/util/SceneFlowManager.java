package de.dfki.vsm.editor.util;

//~--- non-JDK imports --------------------------------------------------------

import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SceneFlow;
import de.dfki.vsm.model.sceneflow.SuperNode;

//~--- JDK imports ------------------------------------------------------------

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;
import java.util.Vector;

/**
 * @author Not me
 * @author Patrick Gebhard
 */
public class SceneFlowManager {
    private final SceneFlow             mSceneFlow;
    private final IDManager             mIDManager;
    private final LinkedList<SuperNode> mActiveSuperNodes;

    public SceneFlowManager(SceneFlow sceneFlow) {
        mSceneFlow        = sceneFlow;
        mIDManager        = new IDManager(mSceneFlow);
        mActiveSuperNodes = new LinkedList<SuperNode>();
        mActiveSuperNodes.addLast(mSceneFlow);
    }

    public SceneFlow getSceneFlow() {
        return mSceneFlow;
    }

    public IDManager getIDManager() {
        return mIDManager;
    }

    public SuperNode getCurrentActiveSuperNode() {
        return mActiveSuperNodes.getLast();
    }

    public LinkedList<SuperNode> getActiveSuperNodes() {
        return mActiveSuperNodes;
    }

    public void addActiveSuperNode(SuperNode value) {
        mActiveSuperNodes.addLast(value);
    }

    public SuperNode removeActiveSuperNode() {
        return mActiveSuperNodes.removeLast();
    }

    public boolean isRootSuperNode(Node n) {
        return (n.equals((SuperNode) mSceneFlow));
    }

    /*
     * Returns a set of Node IDs that are alternative Startnodes of a SuperNode
     */
    public Set<String> getAlternativeStartNode(SuperNode superNode) {
        Set<String> altStartNodeIDs = new HashSet<String>();

        if (!(superNode instanceof SceneFlow)) {
            SuperNode    parentSuperNode = getParentSuperNode(superNode);
            Vector<Node> parentNodeSet   = parentSuperNode.getNodeList();
            Set<String>  currentNodeIDs  = new HashSet<String>();

            for (Node cn : superNode.getNodeList()) {
                currentNodeIDs.add(cn.getId());
            }

            for (Node node : parentNodeSet) {
                if (node.hasEdge()) {
                    switch (node.getFlavour()) {
                    case CNODE :
                        Vector<CEdge> ces = node.getCEdgeList();

                        for (CEdge c : ces) {

                            // collectAltStartNodeIDs(processIDs(c.getStart()), currentNodeIDs, altStartNodeIDs);
                        }

                        break;

                    case PNODE :
                        Vector<PEdge> pes = node.getPEdgeList();

                        for (PEdge p : pes) {

                            // collectAltStartNodeIDs(processIDs(p.getStart()), currentNodeIDs, altStartNodeIDs);
                        }

                        break;

                    case INODE :
                        Vector<IEdge> ies = node.getIEdgeList();

                        for (IEdge i : ies) {

                            // collectAltStartNodeIDs(processIDs(i.getStart()), currentNodeIDs, altStartNodeIDs);
                        }

                        break;

                    case NONE :
                        if (node.hasDEdge()) {

                            // collectAltStartNodeIDs(processIDs(node.getDedge().getStart()), currentNodeIDs, altStartNodeIDs);
                        }

                        break;
                    }
                }
            }
        }

        return altStartNodeIDs;
    }

    /*
     * Returns the list of all parent SuperNodes containing the root SuperNode
     */
    public Set<SuperNode> getParentSuperNodeSet(Node n) {
        Set<SuperNode> nSet = new HashSet<SuperNode>();

        if (isRootSuperNode(n)) {    // if given node n is root SuperNode return null
            return null;
        } else {
            SuperNode sn = getParentSuperNode(n);

            if (sn != null) {
                nSet.add(sn);

                if (!sn.equals((SuperNode) mSceneFlow)) {
                    nSet = buildSuperNodeSet(sn, nSet);
                }
            }
        }

        return (nSet.size() > 0)
               ? nSet
               : null;
    }

    /*
     * Helper method for the recursive process of building the set of parent
     * SuperNodes to a given Node.
     */
    private Set<SuperNode> buildSuperNodeSet(SuperNode sn, Set<SuperNode> nSet) {
        SuperNode pn = getParentSuperNode(sn);

        if (pn != null) {
            nSet.add(pn);

            if (!pn.equals((SuperNode) mSceneFlow)) {
                nSet = buildSuperNodeSet(pn, nSet);
            }
        }

        return nSet;
    }

    /*
     * Returns the parent SuperNode to a given Node n
     */
    public SuperNode getParentSuperNode(Node n) {
        if (!isRootSuperNode(n)) {
            SuperNode parentSuperNode = (SuperNode) mSceneFlow;
            Set<Node> ns              = getSubNodes(parentSuperNode);

            // checking if node is contained in the nodes of the root SuperNode
            for (Node cn : ns) {
                if (cn.equals(n)) {
                    return parentSuperNode;
                } else {
                    if (SuperNode.class.isInstance(cn)) {
                        SuperNode sun = findParentSuperNode((SuperNode) cn, n);

                        if (sun != null) {
                            return sun;
                        }
                    }
                }
            }
        }

        // return null if no parent (super) node exists
        return null;
    }

    /*
     * Helper method for recursive traversion of supernodes to find Parent SuperNode to given Node
     */
    private SuperNode findParentSuperNode(SuperNode currentSN, Node n) {
        if (hasSuperNodes(currentSN)) {
            SuperNode parentSuperNode = currentSN;
            Set<Node> ns              = getSubNodes(currentSN);

            for (Node cn : ns) {
                if (cn.equals(n)) {
                    return parentSuperNode;
                } else {
                    if (SuperNode.class.isInstance(cn)) {
                        SuperNode sun = findParentSuperNode((SuperNode) cn, n);

                        if (sun != null) {
                            return sun;
                        }
                    }
                }
            }
        }

        return null;
    }

    /*
     * Checks if a given Node instance is a subnode of a given intance of SuperNode
     */
    public boolean isSubNode(Node superNode, Node n) {
        if (!(superNode instanceof SuperNode)) {
            return false;
        }

        Set<Node> nSet = getSuperNodeSubNodes((SuperNode) superNode, new HashSet<Node>());

        // DEBUG //System.out.println("super node set size " + nSet.size());
        if ((nSet == null) || (nSet.size() == 0)) {
            return false;
        }

        if (nSet.contains(n)) {
            return true;
        } else {
            return false;
        }
    }

    private Set<Node> getSuperNodeSubNodes(SuperNode sNode, Set allSubNodes) {

        // get all super nodes and nodes
        Vector<Node>      ns  = sNode.getNodeList();         // .getNodeSet();
        Vector<SuperNode> sns = sNode.getSuperNodeList();    // .getSuperNodeSet();

        // add super nodes and nodes to one set
        for (SuperNode sn : sns) {
            allSubNodes.add(sn);
            getSuperNodeSubNodes(sn, allSubNodes);    // recurvsively collect all SubSupernodes
        }

        for (Node n : ns) {
            allSubNodes.add(n);
        }

        return allSubNodes;
    }

    /*
     * Checks if a given SuperNode contains an instance of SuperNode an returns a appropriate
     * boolean value
     *
     * @param SuperNode
     */
    private boolean hasSuperNodes(SuperNode sn) {

        // return (sn.getSuperNodeSet().size() > 0) ? true : false;
        return (sn.getSuperNodeList().size() > 0);
    }

    public Set<Node> getSubNodes() {
        return getSubNodes((SuperNode) mSceneFlow);
    }

    public Set<Node> getSubNodes(SuperNode sNode) {
        HashSet<Node> allNodes = new HashSet<Node>();

        // get all super nodes and nodes
        // Set<Node> ns = sNode.getNodeSet();
        // Set<SuperNode> sns = sNode.getSuperNodeSet();
        Vector<Node>      ns  = sNode.getNodeList();         // .getNodeSet();
        Vector<SuperNode> sns = sNode.getSuperNodeList();    // .getSuperNodeSet();

        // add super nodes and nodes to one set
        for (SuperNode sn : sns) {
            allNodes.add(sn);
        }

        for (Node n : ns) {
            allNodes.add(n);
        }

        return allNodes;
    }

    public Set<String> getSubNodesNames() {
        HashSet<String> allNodeNames = new HashSet<String>();

        // get all active super nodes and nodes
        // Set<Node> ns = mActiveSuperNodes.getLast().getNodeSet();
        // Set<SuperNode> sns = mActiveSuperNodes.getLast().getSuperNodeSet();
        Vector<Node>      ns  = mActiveSuperNodes.getLast().getNodeList();         // .getNodeSet();
        Vector<SuperNode> sns = mActiveSuperNodes.getLast().getSuperNodeList();    // .getSuperNodeSet();

        // add super nodes and nodes to one set
        for (SuperNode sn : sns) {
            allNodeNames.add(sn.getName() + " (" + sn.getId() + ")");
        }

        for (Node n : ns) {
            allNodeNames.add(n.getName() + " (" + n.getId() + ")");
        }

        return allNodeNames;
    }

//  public String getSceneFlowFileName() {
//      return mSceneFlowFileName;
//  }
//
//  public String getSceneFlowFilePath() {
//      if (mSceneFlowFile != null) {
//          return mSceneFlowFile.getPath();
//      } else {
//          return "<untitled>";
//      }
//  }
//
//  public void setSceneFlowFileName(String value) {
//      mSceneFlowFileName = value;
//  }
//
//  public void setSceneFlowFile(File value) {
//      mSceneFlowFile = value;
//  }
//
//  public File getSceneFlowFile() {
//      return mSceneFlowFile;
//  }
//  public void setSceneFlow(SceneFlow value) {
//      mSceneFlow = value;
//      mActiveSuperNodes.addLast(mSceneFlow);
//  }
//  public boolean hasChangedSinceLastSave() {
//      if (mSceneFlowFile == null) {
//          return true;
//      }
//      File file = new File(mSceneFlowFile.getParent() + File.separator + "~" + mSceneFlowFile.getName());
//      boolean hasChanged = false;
//      try {
//          IndentOutputStream out = new IndentOutputStream(file);
//          mSceneFlow.writeXML(out);
//          out.close();
//          hasChanged = !FileAttributes.compare(file, mSceneFlowFile);
//      } catch (IOException e) {
//          e.printStackTrace();
//          return true;
//      } finally {
//          try {
//              file.delete();
//              file = null;
//          } catch (SecurityException e) {
//              e.printStackTrace();
//          }
//      }
//      return hasChanged;
//  }
}
