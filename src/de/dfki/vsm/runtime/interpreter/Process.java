package de.dfki.vsm.runtime.interpreter;

//~--- non-JDK imports --------------------------------------------------------
import de.dfki.vsm.editor.event.EdgeExecutedEvent;
import de.dfki.vsm.editor.event.NodeExecutedEvent;
import de.dfki.vsm.editor.event.NodeStartedEvent;
import de.dfki.vsm.editor.event.NodeTerminatedEvent;
import de.dfki.vsm.model.sceneflow.CEdge;
import de.dfki.vsm.model.sceneflow.EEdge;
import de.dfki.vsm.model.sceneflow.Edge;
import de.dfki.vsm.model.sceneflow.FEdge;
import de.dfki.vsm.model.sceneflow.IEdge;
import de.dfki.vsm.model.sceneflow.Node;
import de.dfki.vsm.model.sceneflow.PEdge;
import de.dfki.vsm.model.sceneflow.SuperNode;
import de.dfki.vsm.model.sceneflow.TEdge;
import de.dfki.vsm.model.sceneflow.command.Command;
import de.dfki.vsm.model.sceneflow.definition.VarDef;
import de.dfki.vsm.runtime.exceptions.InterruptException;
import de.dfki.vsm.runtime.exceptions.InterpretException;
import de.dfki.vsm.runtime.exceptions.TerminateException;
import de.dfki.vsm.runtime.events.AbortionEvent;
import de.dfki.vsm.runtime.values.BooleanValue;
import de.dfki.vsm.util.evt.EventDispatcher;
import de.dfki.vsm.util.log.LOGDefaultLogger;

//~--- JDK imports ------------------------------------------------------------
import java.util.Collection;
import java.util.Random;
import java.util.Vector;

/**
 * @author Not me
 */
public class Process extends java.lang.Thread {

	private boolean mIsPauseRequested = false;
	private boolean mIsInterruptionRequested = false;
	private boolean mIsTerminationRequested = false;
	private boolean mIsRunning = false;
	private boolean mWasExecuted = false;
	private Node mCurrentNode = null;
	private Edge mInterruptEdge = null;
	private Edge mNextEdge = null;
	private Edge mIncomingEdge = null;
	private final Vector<Process> mChildThreadList = new Vector<Process>();
	private final Vector<Process> mAddChildThreadList = new Vector<Process>();
	private final LOGDefaultLogger mLogger = LOGDefaultLogger.getInstance();
	private final Configuration mConfiguration;
	private final SystemHistory mSystemHistory;
	private final Evaluator mEvaluator;
	private final EventObserver mEventObserver;
	private final TimeoutManager mTimeoutManager;
	private final Environment mEnvironment;
	private final int mLevel;
	private long mNodeTime;
	private final Process mParentThread;
	private Interpreter mInterpreter;

	public Process(String name, ThreadGroup group, Node currentNode, Environment environment, int level,
	  Process parent, Interpreter interpreter) {
		super(group, name);
		mCurrentNode = currentNode;
		mEnvironment = environment;
		mLevel = level;
		mParentThread = parent;
		mInterpreter = interpreter;

		/**
		 * Make a new symbol table for the current node
		 */
		mEnvironment.push();
		mConfiguration = mInterpreter.getConfiguration();
		mSystemHistory = mInterpreter.getSystemHistory();
		mEvaluator = mInterpreter.getEvaluator();
		mEventObserver = mInterpreter.getEventObserver();
		mTimeoutManager = mInterpreter.getTimeoutManager();
	}

	public int getLevel() {
		return mLevel;
	}

	public Node getNode() {
		return mCurrentNode;
	}

	public Process getParentThread() {
		return mParentThread;
	}

	public Environment getEnvironment() {
		return mEnvironment;
	}

	public void checkStatus() throws TerminateException, InterruptException {
		checkPaused();

		if (mIsTerminationRequested) {
			throw new TerminateException(this, null);
		}

		if (mIsInterruptionRequested) {
			throw new InterruptException(this, null);
		}
	}

	public void requestPause() {
		if (mIsPauseRequested) {
			return;
		}

		for (Process thread : mChildThreadList) {
			thread.requestPause();
		}

		for (Process thread : mAddChildThreadList) {
			thread.requestPause();
		}

		mIsPauseRequested = true;
	}

	public void requestProceed() {
		if (!mIsPauseRequested) {
			return;
		}

		for (Process thread : mChildThreadList) {
			thread.requestProceed();
		}

		for (Process thread : mAddChildThreadList) {
			thread.requestProceed();
		}

		mIsPauseRequested = false;
		mInterpreter.signalAll();
	}

	public boolean isPauseRequested() {
		return mIsPauseRequested;
	}

	private void checkPaused() {
		while (mIsPauseRequested) {
			mInterpreter.await();
		}
	}

	public synchronized boolean isRunning() {
		return mIsRunning;
	}

	public synchronized boolean wasExecuted() {
		return mWasExecuted;
	}

	public void requestTermination() {
		if (mIsTerminationRequested) {
			return;
		}

		for (Process thread : mChildThreadList) {
			thread.requestTermination();
		}

		for (Process thread : mAddChildThreadList) {
			thread.requestTermination();
		}

		// mLogger.message("Interpreter: Process " + Process.currentThread().getName() + " requests termination of thread " + getName());
		mIsTerminationRequested = true;
		interrupt();
	}

	public void requestInterruption(Edge edge) {
		if (mIsInterruptionRequested || mIsTerminationRequested) {
			return;
		}

		for (Process thread : mChildThreadList) {
			thread.requestTermination();
		}

		for (Process thread : mAddChildThreadList) {
			thread.requestTermination();
		}

		// mLogger.message("Interpreter: Process " + Process.currentThread().getName() + " requests interruption of thread " + getName());
		mInterruptEdge = edge;
		mIsInterruptionRequested = true;
		interrupt();
	}

	public void handleStart() throws InterpretException {

		/**
		 * Set the new current node start time
		 */
		mNodeTime = System.currentTimeMillis();

		/**
		 * Update the configuration by adding the new current node to it
		 */
		mConfiguration.enterState(new Configuration.State(mCurrentNode, this));

		/**
		 * Create a new temporary history entry for the new current node
		 */
		mSystemHistory.set(mCurrentNode, new SystemHistory.Entry(mCurrentNode));

		/**
		 * Initialize all timeout conditions of the new current node and
		 * outgoing edges
		 */
		processTimeoutConditionList();

		/**
		 * Process the variable definitions of the new current node
		 */
		processVarDefList();

		/**
		 * Start the thread
		 */
		super.start();

		/**
		 * Set flag that sceneflow is running
		 */
		mIsRunning = true;

		/**
		 * Set flag a sceneflow was executed to false, since it will be
		 */
		mWasExecuted = false;

		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new NodeStartedEvent(this, mCurrentNode));
	}

	public void handleTermination() throws InterpretException {
		if (mParentThread != null) {

			/**
			 * Update the history entry of the parent thread
			 */
			mSystemHistory.get(mParentThread.mCurrentNode).addChildNode(mCurrentNode);
		}

		/**
		 * Update the history entry with the current symbol table and the curent
		 * time
		 */
		mSystemHistory.get(mCurrentNode).setSymbolTable(mEnvironment.getFirst());    // TODO: POP?
		mSystemHistory.get(mCurrentNode).setEndTime();
		mSystemHistory.push(mCurrentNode, mSystemHistory.get(mCurrentNode));

		/**
		 * Update the configuration by removing the current node from it
		 */
		mConfiguration.exitState(mCurrentNode, this);

		/**
		 * Reset the thread data
		 */
		mIsTerminationRequested = false;
		mIsInterruptionRequested = false;
		mIsPauseRequested = false;
		mNextEdge = null;
		mInterruptEdge = null;
		mIncomingEdge = null;
		interrupted();

		// PathLogger.deregister(this);
		/**
		 * Set flag that sceneflow is terminated
		 */
		mIsRunning = false;

		/**
		 * Set flag a sceneflow was executed
		 */
		mWasExecuted = true;

		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new NodeTerminatedEvent(this, mCurrentNode));
	}

	public void handleForkTermination() throws InterpretException {

		/**
		 * Update the history entry with the current symbol table and the curent
		 * time
		 */
		mSystemHistory.get(mCurrentNode).setSymbolTable(mEnvironment.getFirst());    // TODO: POP?
		mSystemHistory.get(mCurrentNode).setEndTime();
		mSystemHistory.push(mCurrentNode, mSystemHistory.get(mCurrentNode));

		/**
		 * Update the configuration by removing the current node from it
		 */
		mConfiguration.exitState(mCurrentNode, this);

		/**
		 * Reset the thread data
		 */
		mIsTerminationRequested = false;
		mIsInterruptionRequested = false;
		mIsPauseRequested = false;
		mNextEdge = null;
		mInterruptEdge = null;
		mIncomingEdge = null;
		interrupted();

		// PathLogger.deregister(this);
		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new NodeTerminatedEvent(this, mCurrentNode));
	}

	public void handleInterruption() throws InterpretException {

		/**
		 * Set the new incoming edge to the interruptive edge that caused this
		 * interruption
		 */
		mIncomingEdge = mInterruptEdge;

		/**
		 * Update the history entry with the current symbol table and the curent
		 * time
		 */
		mSystemHistory.get(mCurrentNode).setSymbolTable(mEnvironment.pop());
		mSystemHistory.get(mCurrentNode).setEndTime();
		mSystemHistory.push(mCurrentNode, mSystemHistory.get(mCurrentNode));

		/**
		 * Update the configuration by removing the current node from it
		 */
		mConfiguration.exitState(mCurrentNode, this);

		/**
		 * Set the new current node to the target node of the incoming edge
		 */
		mCurrentNode = mIncomingEdge.getTargetNode();

		/**
		 * Initialize the new current environment with the empty environment
		 */
		mEnvironment.push();

		/**
		 * Set the new current node start time
		 */
		mNodeTime = System.currentTimeMillis();

		/**
		 * Update the configuration by adding the new current node to it
		 */
		mConfiguration.enterState(new Configuration.State(mCurrentNode, this));

		/**
		 * Create a new temporary history entry for the new current node
		 */
		mSystemHistory.set(mCurrentNode, new SystemHistory.Entry(mCurrentNode));

		/**
		 * Initialize all timeout conditions of the new current node and
		 * outgoing edges
		 */
		processTimeoutConditionList();

		/**
		 * Process the variable definitions of the new current node
		 */
		processVarDefList();

		/**
		 * Process the on exit commands of the new current node
		 */
		// processOnExitCommandList();
		/**
		 * Reset the thread data
		 */
		mIsTerminationRequested = false;
		mIsInterruptionRequested = false;
		mInterruptEdge = null;
		mNextEdge = null;
		interrupted();

		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new EdgeExecutedEvent(this, mIncomingEdge));
		EventDispatcher.getInstance().convey(new NodeStartedEvent(this, mCurrentNode));
	}

	public void handleContinuation() throws InterpretException {

		/**
		 * Set the new incoming edge to the next edge that has to be executed
		 */
		mIncomingEdge = mNextEdge;

		// PathLogger.logEdge(mIncomingEdge, this);
		/**
		 * Update the history entry with the current symbol table and the curent
		 * time
		 */
		mSystemHistory.get(mCurrentNode).setSymbolTable(mEnvironment.pop());
		mSystemHistory.get(mCurrentNode).setEndTime();
		mSystemHistory.push(mCurrentNode, mSystemHistory.get(mCurrentNode));

		/**
		 * Update the configuration by removing the current node from it
		 */
		mConfiguration.exitState(mCurrentNode, this);

		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new NodeExecutedEvent(this, mCurrentNode));

		/**
		 * Set the new current node to the target node of the incoming edge
		 */
		mCurrentNode = mIncomingEdge.getTargetNode();

		/**
		 * Initialize the new current environment with the empty environment
		 */
		mEnvironment.push();

		/**
		 * Set the new current node start time
		 */
		mNodeTime = System.currentTimeMillis();

		/**
		 * Update the configuration by adding the new current node to it
		 */
		mConfiguration.enterState(new Configuration.State(mCurrentNode, this));

		/**
		 * Create a new temporary history entry for the new current node
		 */
		mSystemHistory.set(mCurrentNode, new SystemHistory.Entry(mCurrentNode));

		/**
		 * Initialize all timeout conditions of the new current node and
		 * outgoing edges
		 */
		processTimeoutConditionList();

		/**
		 * Process the variable definitions of the new current node
		 */
		processVarDefList();

		/**
		 * Process the on exit commands of the new current node
		 */
		// processOnExitCommandList();
		/**
		 * Reset the thread data
		 */
		mIsTerminationRequested = false;
		mIsInterruptionRequested = false;
		mInterruptEdge = null;
		mNextEdge = null;
		interrupted();

		/**
		 * Multicast the events for visualization
		 */
		EventDispatcher.getInstance().convey(new EdgeExecutedEvent(this, mIncomingEdge));
		EventDispatcher.getInstance().convey(new NodeStartedEvent(this, mCurrentNode));
	}

	@Override
	public void run() {

		// PathLogger.register(this);
		try {
			execute();
		} catch (InterpretException e) {

			// PathLogger.deregister(this);
			/**
			 * Multicast the events for visualization
			 */
			// mLogger.message("Interpreter: Aborting execution");
			EventDispatcher.getInstance().convey(new AbortionEvent(this, e));

			/**
			 * Stop the interpreter
			 */
			mInterpreter.abort();
			mInterpreter.unlock();
		}
	}

	private void execute() throws InterpretException {
		mInterpreter.lock();

		try {
			checkStatus();
		} catch (InterruptException e) {

			// mLogger.message("Interpreter: Process " + getName() + " has catched an interruption request");
			handleInterruption();
		} catch (TerminateException e) {

			// mLogger.message("Interpreter: Process " + getName() + " has catched a termination request");
			handleTermination();
			mInterpreter.unlock();

			return;
		}

		while (true) {

			/**
			 * Start the event observer to detect the changes that have been
			 * introduced by the change of the configuration and the
			 * initialization of the nodes that have been entered to the
			 * configuration.
			 */
			mEventObserver.update();
			mInterpreter.unlock();

			try {

				// PathLogger.logNode(mCurrentNode, this);
				// checkPaused();
				/////////////////////////////////////////////////////////////
				// Process command list
				////////////////////////////////////////////////////////////
				for (Command cmd : mCurrentNode.getCmdList()) {
					mInterpreter.lock();
					checkStatus();

					/**
					 * Execute the command and update the history
					 */
					mEvaluator.execute(cmd, mEnvironment);
					mSystemHistory.get(mCurrentNode).addCmd(cmd);

					/**
					 * Check interruptive edges
					 */
					mEventObserver.update();
					checkStatus();
					mInterpreter.unlock();
				}

				/////////////////////////////////////////////////////////////
				// EXECUTE START NODE LIST
				////////////////////////////////////////////////////////////
				if (mCurrentNode instanceof SuperNode) {
					executeStartNodeList();
				}

				/////////////////////////////////////////////////////////////
				// FIND NEXT EDGE
				////////////////////////////////////////////////////////////
				Vector<FEdge> nextEdgeList = mCurrentNode.getFEdgeList();

				if (nextEdgeList.isEmpty()) {

					/////////////////////////////////////////////////////////////
					// EXECUTE REGULAR NEXT EDGE
					////////////////////////////////////////////////////////////
					Edge nextEdge = null;

					while (nextEdge == null) {
						try {
							Process.sleep(10);
						} catch (InterruptedException e) {
						}

						mInterpreter.lock();
						checkStatus();

						////////////////////////
						// Probabilistic edge
						nextEdge = checkPEdgeList();

						if (nextEdge != null) {
							break;
						}

						// Conditional edge
						nextEdge = checkCEdgeList();

						if (nextEdge != null) {
							break;
						}

						// Default edge
						Edge dedge = mCurrentNode.getDedge();

						if (dedge instanceof TEdge) {
							TEdge tedge = (TEdge) dedge;

							if ((java.lang.System.currentTimeMillis() - mNodeTime) >= tedge.getTimeout()) {
								nextEdge = tedge;

								break;
							} else {
								nextEdge = null;
								mInterpreter.unlock();

								continue;
							}
						} else if (dedge instanceof EEdge) {
							EEdge eedge = (EEdge) dedge;

							nextEdge = eedge;

							break;
						} else {

							/**
							 * There can neither be found a conditional edge nor
							 * a default edge that can be taken. Therefore the
							 * current node is a possible end node and this
							 * method returns with a null pointer. But there is
							 * still the possibility, that this thread has
							 * received a request for termination or
							 * interruption since the last check of it's status
							 * or that it will receive such a request until this
							 * thread is allowed to savely change the
							 * configuration. If it has received such a request
							 * then this request will be detected in the run
							 * method after it has acquired the configuration
							 * lock and it's own lock. If such a request will
							 * then be detected at tis point, then it does not
							 * matter if this method returned with a valid edge
							 * or a null pointer, because then this request will
							 * be handled with higher priority and
							 * TODO???????????????
							 *
							 */
							break;
						}
					}

					////////////////////////
					mNextEdge = nextEdge;

					if (mNextEdge != null) {
						handleContinuation();

						continue;
					} else {
						handleTermination();

						break;
					}
				} /////////////////////////////////////////////////////////////
				// EXECUTE FORK EDGE LIST
				////////////////////////////////////////////////////////////
				else {
					mInterpreter.lock();
					checkStatus();

					Vector<Process> forkThreadList = new Vector<Process>();

					for (Edge edge : nextEdgeList) {
						Environment env = mParentThread.mEnvironment.getCopy();
						Process thread = new Process(edge.getTargetNode().getId(), null, edge.getTargetNode(), env,
						  mLevel, mParentThread, mInterpreter);

						// TODO: constructor incoming edge
						mParentThread.mAddChildThreadList.add(thread);
						forkThreadList.add(thread);
						thread.handleStart();
						EventDispatcher.getInstance().convey(new EdgeExecutedEvent(this, edge));

						// Propagate the pause status
						thread.mIsPauseRequested = mIsPauseRequested;
					}

					handleForkTermination();

					/* *Wake up the parent thread so that the parent thread can update its child list */
					mParentThread.interrupt();

					/**
					 *
					 */
					mEventObserver.update();
					mInterpreter.unlock();

					return;
				}
			} ////////////////////////////////////////////////////////////
			/////////////////////////////////////////////////////////////
			// HANDLE INTERRUPTION REQUESTS AND TERMINATIOJ REQUESTS
			////////////////////////////////////////////////////////////
			catch (InterruptException e) {

				// mLogger.message("Interpreter: Process " + getName() + " catched an interruption request");
				handleInterruption();

				continue;
			} catch (TerminateException e) {

				// mLogger.message("Interpreter: Process " + getName() + " catched a termination request");
				handleTermination();

				break;
			}
		}

		// mLogger.message("Interpreter: Process " + getName() + " has been terminated in node " + mCurrentNode.getId());
		mInterpreter.unlock();
	}

	/**
	 */
	private void executeStartNodeList() throws InterruptException, TerminateException, InterpretException {
		Vector<Node> startNodeList = computeStartNodeList();

		if (!startNodeList.isEmpty()) {
			mInterpreter.lock();
			checkStatus();

			/**
			 *
			 */
			for (Node node : startNodeList) {
				Process thread = new Process(node.getId(), null, node, mEnvironment.getCopy(), mLevel + 1, this,
				  mInterpreter);

				mChildThreadList.add(thread);
				thread.handleStart();
			}

			/**
			 * Here the changes to the configuration are finally performed. The
			 * child threads are added to the configuration and are started. Now
			 * this thread can release the write lock of the configuration. This
			 * change is a change in the system state and it could be the case
			 * that there exists an interruptive edge whose condition evaluates
			 * to true now. For this reason we have to start the event observer
			 * ...
			 */
			mEventObserver.update();
			checkStatus();    // ERROR if here terminatedRequested and lock holding then never releasing lock again
			mInterpreter.unlock();

			/**
			 * Try to wait until all child threads are terminated. If this
			 * thread is interrupted while waiting for a child thread, then it
			 * tries again to wait. So this thread can just continue execution
			 * when all its child threads have been terminated their execution.
			 */
			boolean allChildThreadsAreTerminated = false;

			while (!allChildThreadsAreTerminated) {
				try {
					for (Process thread : mChildThreadList) {
						thread.join();
					}
				} catch (InterruptedException e) {

					/**
					 * If this thread is interrupted while it waits within the
					 * join call then we have to repeat the joining procedure
					 * until all it's child threads are terminated. The
					 * interrupted flag of the current thread is thereby cleared
					 * by the throwing of the InterruptedException.
					 */
					mInterpreter.lock();
					updateChildList();
					mInterpreter.unlock();

					continue;
				}

				/**
				 * Egal was passiert , wir warten bis alle childs beendet sind,
				 * erst dann wird der status gecheckt
				 *
				 */
				mInterpreter.lock();

				if (mAddChildThreadList.isEmpty()) {

					/**
					 * If this thread has successfully waited for the
					 * termination of all it's child threads then it sets it's
					 * activity status in the configuration to active again.
					 */
					allChildThreadsAreTerminated = true;
					mChildThreadList.clear();
				} else {
					updateChildList();
				}

				mInterpreter.unlock();
			}

			/**
			 * We have to check the status here to ckeck if this thread was
			 * terminated or interrupted or if this thread has to continue the
			 * execution regularily.
			 */
			// mLogger.message("Interpreter: All child threads of thread " + getName() + " have been terminated");
			mInterpreter.lock();
			checkStatus();
			mInterpreter.unlock();
		}
	}

	private void updateChildList() {

		/**
		 * Create a list with already dead childs
		 */
		Vector<Process> deadChildList = new Vector<Process>();

		for (Process thread : mChildThreadList) {
			if (!thread.isAlive()) {
				deadChildList.add(thread);
			}
		}

		/**
		 * Remove the dead childs from the child list
		 */
		for (Process thread : deadChildList) {
			mChildThreadList.remove(thread);
		}

		/**
		 * Add the living additional childs to the child list
		 */
		for (Process thread : mAddChildThreadList) {
			if (thread.isAlive()) {
				mChildThreadList.add(thread);
			}
		}

		/**
		 * Clear the additional child list
		 */
		mAddChildThreadList.clear();
	}

	/**
	 */
	private Vector<Node> computeStartNodeList() {

		/**
		 */
		Vector<Node> startNodeList = new Vector<Node>();

		/**
		 * Get the history node of the current node
		 */
		Node historyNode = ((SuperNode) mCurrentNode).getHistoryNode();

		/**
		 * Look if there exists a history for the current node
		 */
		if (!mSystemHistory.isEmpty(mCurrentNode)) {

			/**
			 * The history of the current node is not empty, so that we have
			 * some history information of the current node. In this case we do
			 * consider the history node and check if there exist any edges
			 * originating from the history node.
			 */
			if (historyNode != null) {
				if (!historyNode.getEdgeList().isEmpty()) {
					startNodeList.add(historyNode);

					return startNodeList;
				}
			}
		}

		/**
		 * There do not exist any edges originating from the history node or the
		 * history of the current node is empty, so that we do not have any
		 * history information of the current node. In this case we do ignore
		 * the history node and choose the ordinary start nodes and the
		 * alternative start nodes of the current node as the next nodes to
		 * execute.
		 */
		/**
		 * Get the start node list of the current node
		 */
		Collection<Node> commonStartNodeList = ((SuperNode) mCurrentNode).getStartNodeMap().values();

		/**
		 * Fill the next start node list with these start nodes
		 */
		for (Node node : commonStartNodeList) {
			startNodeList.add(node);
		}

		/**
		 * Check if there exist any alternative start nodes. In this case
		 * replace the start nodes with their substitutions.
		 */
//      if (mIncomingEdge != null) {
//        Iterator it = mIncomingEdge.getAltStartNodeMap().entrySet().iterator();
//        while (it.hasNext()) {
//          Map.Entry pairs = (Map.Entry) it.next();
//          Pair<String, Node> startNodePair = (Pair<String, Node>) pairs.getKey();
//          Pair<String, Node> altStartNodePair = (Pair<String, Node>) pairs.getValue();
//          if (startNodePair.getSecond() == null) {
//            startNodeList.add(altStartNodePair.getSecond());
//          } else {
//            if (commonStartNodeList.contains(startNodePair.getSecond())) {
//              startNodeList.remove(startNodePair.getSecond());
//              startNodeList.add(altStartNodePair.getSecond());
//            }
//          }
//        }
//      }
		return startNodeList;
	}

	/**
	 */
	private void processVarDefList() throws InterpretException {
		for (VarDef varDef : mCurrentNode.getVarDefList()) {
			mEvaluator.declare(varDef, mEnvironment);
		}
	}

	/**
	 */
	private void processTimeoutConditionList() throws InterpretException {
		for (VarDef varDef : mCurrentNode.getVarDefList()) {
			mTimeoutManager.startTimeoutHandler(varDef, mEnvironment);
		}

		for (Command cmd : mCurrentNode.getCmdList()) {
			mTimeoutManager.startTimeoutHandler(cmd, mEnvironment);
		}

		for (CEdge cedge : mCurrentNode.getCEdgeList()) {
			mTimeoutManager.startTimeoutHandler(cedge.getCondition(), mEnvironment);
		}

		for (IEdge iedge : mCurrentNode.getIEdgeList()) {
			mTimeoutManager.startTimeoutHandler(iedge.getCondition(), mEnvironment);
		}
	}

	/**
	 */
	private CEdge checkCEdgeList() throws InterpretException {
		for (CEdge cedge : mCurrentNode.getCEdgeList()) {
			try {
				BooleanValue value = (BooleanValue) mEvaluator.evaluate(cedge.getCondition(), mEnvironment);

				// mLogger.message(cedge.getCondition().getConcreteSyntax() + " evaluates to " + value.getValue());
				if (value.getValue()) {
					return cedge;
				}
			} catch (ClassCastException e) {
				java.lang.String errorMsg = "An error occured while executing thread "
				  + Process.currentThread().toString() + " : " + "The condition '"
				  + cedge.getCondition().getConcreteSyntax()
				  + "' of the conditional edge from node '" + cedge.getSource()
				  + "' to node '" + cedge.getTarget()
				  + "' could not be evaluated to a boolean value.";

				throw new InterpretException(this, errorMsg);
			}
		}

		return null;
	}

	private PEdge checkPEdgeList() {
		int random = new Random().nextInt(100);
		int low = 0,
		  high = 0;

		for (PEdge pedge : mCurrentNode.getPEdgeList()) {
			high = low + pedge.getProbability() - 1;

			if ((low <= random) && (random <= high)) {
				return pedge;
			}

			low = high + 1;
		}

		return null;
	}
}
