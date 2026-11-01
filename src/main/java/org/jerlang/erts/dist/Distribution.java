package org.jerlang.erts.dist;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.jerlang.Aliases;
import org.jerlang.Process;
import org.jerlang.Tracing;
import org.jerlang.ProcessOrPort;
import org.jerlang.ProcessRegistry;
import org.jerlang.erts.erlang.Error;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.PID;
import org.jerlang.type.PidOrPortId;
import org.jerlang.type.Reference;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.vm.VirtualMachine;

/**
 * The nodes this virtual machine is connected to, and the entry points
 * for sending signals to them.
 *
 * Only outgoing connections are implemented: JErlang can connect to an
 * Erlang/OTP node and exchange messages over that connection, but it does
 * not listen for incoming connections yet.
 */
public final class Distribution {

    private static final Object CONNECT_LOCK = new Object();

    private static final Map<Atom, DistConnection> CONNECTIONS = new ConcurrentHashMap<>();

    private static final Atom DOWN = Atom.of("DOWN");
    private static final Atom PROCESS = Atom.of("process");
    private static final Atom CONNECTED = Atom.of("connected");
    private static final Atom VISIBLE = Atom.of("visible");
    private static final Atom HIDDEN = Atom.of("hidden");
    private static final Atom DISCONNECT = Atom.of("disconnect");

    private Distribution() {
    }

    /**
     * Start the distribution listener for a named node. Nodes without a
     * name can still make outgoing connections, but no node can find them.
     */
    public static void start() {
        DistListener.start();
        DistReconnect.start();
    }

    /**
     * Stop the listener and close every connection.
     */
    public static void shutdown() {
        DistListener.stop();
        DistReconnect.shutdown();
        disconnectAll();
    }

    /**
     * Add a connection accepted by the listener. If an outgoing connection
     * to the same node is already open, keep it and close the new one.
     */
    static void add(DistConnection connection) {
        if (install(connection)) {
            notifyNodeUp(connection);
        }
    }

    /**
     * Install a new connection unless a live one to the same node exists.
     * Returns true when `connection` became the current connection.
     */
    private static boolean install(DistConnection connection) {
        Atom peer = connection.peer();
        while (true) {
            DistConnection existing = CONNECTIONS.putIfAbsent(peer, connection);
            if (existing == null) {
                DistReconnect.connected(peer);
                return true;
            }
            if (existing.isOpen()) {
                connection.close();
                return false;
            }
            if (CONNECTIONS.replace(peer, existing, connection)) {
                DistReconnect.connected(peer);
                return true;
            }
            // Another thread replaced the stale connection first.
        }
    }

    /**
     * Connect to a node, or return true if a connection already exists.
     */
    public static boolean connect(Atom node) {
        if (node == null) {
            return false;
        }
        Atom local = VirtualMachine.instance().node();
        if (!DistNodeName.isValid(local) || !DistNodeName.isValid(node)) {
            return false;
        }
        if (node.equals(local)) {
            return true;
        }
        DistConnection existing = CONNECTIONS.get(node);
        if (existing != null && existing.isOpen()) {
            return true;
        }
        synchronized (CONNECT_LOCK) {
            existing = CONNECTIONS.get(node);
            if (existing != null && existing.isOpen()) {
                return true;
            }
            if (existing != null) {
                CONNECTIONS.remove(node);
            }
            try {
                DistConnection connection = DistConnection.open(node);
                if (install(connection)) {
                    notifyNodeUp(connection);
                }
                return true;
            } catch (IOException | RuntimeException e) {
                if (Boolean.getBoolean("jerlang.dist.debug")) {
                    System.err.println("jerlang dist: can not connect to " + node + ": " + e);
                }
                return false;
            }
        }
    }

    public static boolean isConnected(Atom node) {
        DistConnection connection = CONNECTIONS.get(node);
        return connection != null && connection.isOpen();
    }

    /**
     * The visible nodes this node has a connection to, as erlang:nodes/0
     * reports them. Hidden connections are omitted.
     */
    public static List nodes() {
        return nodes(VISIBLE);
    }

    /**
     * The subset of connected nodes selected by erlang:nodes/1.
     * `connected` includes hidden nodes, `visible` excludes them and
     * `hidden` contains only them.
     */
    public static List nodes(Atom type) {
        boolean all = CONNECTED.equals(type);
        boolean hidden = HIDDEN.equals(type);
        ArrayList<Atom> nodes = new ArrayList<>();
        for (Map.Entry<Atom, DistConnection> entry : CONNECTIONS.entrySet()) {
            DistConnection connection = entry.getValue();
            if (connection.isOpen() && (all || connection.isHidden() == hidden)) {
                nodes.add(entry.getKey());
            }
        }
        return List.of(nodes);
    }

    /**
     * Send `Message` to `Target`, which can be a PID, a registered name,
     * or a {Name, Node} tuple, as the send operator accepts it.
     */
    public static void send(Term target, PID from, Term message) {
        Tracing.send(from, message, target);
        if (target instanceof Reference) {
            // A reference is only a destination while it is an active alias.
            // A send to any other reference is dropped, not an error.
            Aliases.deliver((Reference) target, message);
            return;
        }

        if (target instanceof PID) {
            PID pid = (PID) target;
            if (pid.isLocal()) {
                deliverLocal(pid, message);
            } else {
                sendRemote(pid.node(), pid, null, from, message);
            }
            return;
        }

        if (target instanceof Atom) {
            Atom name = (Atom) target;
            if (ProcessRegistry.instance().get(name) == null) {
                throw Error.badarg;
            }
            deliverLocal(name, message);
            return;
        }

        if (target instanceof Tuple) {
            Tuple tuple = (Tuple) target;
            if (tuple.arity() == 2 && tuple.element(1) instanceof Atom
                && tuple.element(2) instanceof Atom) {
                Atom name = tuple.element(1).toAtom();
                Atom node = tuple.element(2).toAtom();
                if (node.equals(VirtualMachine.instance().node())) {
                    if (ProcessRegistry.instance().get(name) == null) {
                        throw Error.badarg;
                    }
                    deliverLocal(name, message);
                } else {
                    sendRemote(node, null, name, from, message);
                }
                return;
            }
        }

        throw Error.badarg;
    }

    private static void sendRemote(Atom node, PID to, Atom name, PID from, Term message) {
        DistConnection connection = connection(node);
        if (connection == null) {
            // Sending to a node that can not be reached is not an error
            // in Erlang; the message is dropped.
            return;
        }
        try {
            if (to != null) {
                connection.sendPid(to, message);
            } else {
                connection.sendName(name, from != null ? from : new PID(0), message);
            }
        } catch (IOException e) {
            failed(connection, e);
        }
    }

    /**
     * The open connection to a node, connecting on demand. Returns null
     * when the node can not be reached.
     */
    private static DistConnection connection(Atom node) {
        DistConnection connection = CONNECTIONS.get(node);
        if (connection != null && connection.isOpen()) {
            return connection;
        }
        if (!connect(node)) {
            return null;
        }
        connection = CONNECTIONS.get(node);
        return connection != null && connection.isOpen() ? connection : null;
    }

    /**
     * The connection to a node if one is already open. Signals that belong
     * to existing state, such as an exit or a demonitor, do not establish a
     * new connection: a node that just went away must not block the sender
     * in connection setup.
     */
    private static DistConnection openConnection(Atom node) {
        DistConnection connection = CONNECTIONS.get(node);
        return connection != null && connection.isOpen() ? connection : null;
    }

    private static void failed(DistConnection connection, IOException e) {
        if (Boolean.getBoolean("jerlang.dist.debug")) {
            System.err.println("jerlang dist: can not send to " + connection.peer() + ": " + e);
        }
        connection.close();
        DistReconnect.peerLost(connection.peer());
    }

    /**
     * Link a local process to a process on another node. Returns false when
     * the connection can not be established, which erlang:link/1 reports as
     * a noconnection exit signal.
     */
    public static boolean sendLink(PID from, PID to) {
        DistConnection connection = connection(to.node());
        if (connection == null) {
            return false;
        }
        try {
            connection.sendLink(from, to);
            return true;
        } catch (IOException e) {
            failed(connection, e);
            return false;
        }
    }

    public static void sendUnlink(PID from, PID to, long id) {
        DistConnection connection = openConnection(to.node());
        if (connection == null) {
            return;
        }
        try {
            connection.sendUnlink(from, to, id);
        } catch (IOException e) {
            failed(connection, e);
        }
    }

    public static void sendExit(PID from, PID to, Term reason, boolean explicit) {
        // An explicit exit/2 may open a connection, like a send does. The
        // EXIT of a broken link only travels over an existing connection.
        DistConnection connection = explicit ? connection(to.node()) : openConnection(to.node());
        if (connection == null) {
            return;
        }
        try {
            connection.sendExit(from, to, reason, explicit);
        } catch (IOException e) {
            failed(connection, e);
        }
    }

    /**
     * Monitor a process, or a registered name, on another node. `target` is
     * a PID or an atom.
     */
    public static boolean sendMonitor(PID from, Term target, Atom node, Reference reference) {
        DistConnection connection = connection(node);
        if (connection == null) {
            return false;
        }
        try {
            connection.sendMonitor(from, target, reference);
            return true;
        } catch (IOException e) {
            failed(connection, e);
            return false;
        }
    }

    public static void sendDemonitor(PID from, Term target, Atom node, Reference reference) {
        DistConnection connection = openConnection(node);
        if (connection == null) {
            return;
        }
        try {
            connection.sendDemonitor(from, target, reference);
        } catch (IOException e) {
            failed(connection, e);
        }
    }

    /**
     * Send a SPAWN_REQUEST to a node. Returns null on success, or the
     * reason the request could not be sent. A request that could be sent
     * is answered later by a SPAWN_REPLY.
     */
    static Term sendSpawnRequest(PID from, Atom node, Reference requestId, Atom module,
        Atom function, List args, List options) {
        DistConnection connection = connection(node);
        if (connection == null) {
            return Atom.of("noconnection");
        }
        if (!connection.supportsSpawn()) {
            return Atom.of("notsup");
        }
        try {
            connection.sendSpawnRequest(requestId, from, from, module, function, args, options);
            return null;
        } catch (IOException e) {
            failed(connection, e);
            return Atom.of("noconnection");
        }
    }

    public static void sendMonitorExit(Term from, PID to, Reference reference, Term reason) {
        DistConnection connection = openConnection(to.node());
        if (connection == null) {
            return;
        }
        try {
            connection.sendMonitorExit(from, to, reference, reason);
        } catch (IOException e) {
            failed(connection, e);
        }
    }

    /**
     * A peer asked to link to a local process. Returns false when the
     * process is gone; the connection answers with EXIT noproc, like a
     * local link.
     */
    static boolean linkReceived(PID from, PID to) {
        Process target = localProcess(to);
        return target != null && target.linkReceived(from);
    }

    static void unlinkReceived(PID from, PID to) {
        Process target = localProcess(to);
        if (target != null) {
            target.remoteUnlinkReceived(from);
        }
    }

    static void unlinkIdReceived(long id, PID from, PID to) {
        Process target = localProcess(to);
        if (target != null) {
            target.unlinkIdReceived(from, id);
        }
    }

    static void unlinkIdAckReceived(long id, PID from, PID to) {
        Process target = localProcess(to);
        if (target != null) {
            target.unlinkIdAckReceived(from, id);
        }
    }

    static void exitReceived(PID from, PID to, Term reason, boolean explicit) {
        Process target = localProcess(to);
        if (target != null) {
            target.exitReceived(from, reason, explicit);
        }
    }

    /**
     * A peer started monitoring a local PID or name. Returns false when the
     * process is gone; the connection answers with MONITOR_P_EXIT noproc.
     */
    static boolean monitorReceived(PID from, Term to, Reference reference) {
        Process target = monitoredProcess(to);
        Atom name = to instanceof Atom ? (Atom) to : null;
        return target != null && target.addMonitoredBy(reference, from, name);
    }

    static void demonitorReceived(PID from, Term to, Reference reference) {
        Process target = monitoredProcess(to);
        if (target != null) {
            target.removeMonitoredBy(reference);
        }
    }

    /**
     * A monitored process on `peer` terminated, or the name it was
     * registered under disappeared. Monitoring a name reports the object
     * as {Name, Node}, like Erlang/OTP does.
     */
    static void monitorExitReceived(Atom peer, Term from, PID to, Reference reference, Term reason) {
        Process target = localProcess(to);
        if (Boolean.getBoolean("jerlang.dist.debug")) {
            System.err.println("jerlang dist: MONITOR_P_EXIT " + from + " to " + to
                + " ref " + reference + " reason " + reason
                + (target == null ? " without a process" : ""));
        }
        if (target == null) {
            return;
        }
        if (!target.removeMonitor(reference)) {
            if (Boolean.getBoolean("jerlang.dist.debug")) {
                System.err.println("jerlang dist: no such monitor " + reference);
            }
            // The monitor is gone, for example because this node sent a
            // DEMONITOR_P just before the peer's signal arrived.
            return;
        }
        Term object = from instanceof Atom ? Tuple.of(from, peer) : from;
        target.send(Tuple.of(DOWN, reference, PROCESS, object, reason));
    }

    private static Process localProcess(PID pid) {
        ProcessOrPort process = ProcessRegistry.resolve(pid);
        return process instanceof Process ? (Process) process : null;
    }

    private static Process monitoredProcess(Term target) {
        if (target instanceof PID) {
            return localProcess((PID) target);
        }
        if (target instanceof Atom) {
            PidOrPortId id = ProcessRegistry.instance().get((Atom) target);
            ProcessOrPort process = id == null ? null : ProcessRegistry.resolve(id);
            return process instanceof Process ? (Process) process : null;
        }
        return null;
    }

    static boolean hasLocal(PID pid) {
        return ProcessRegistry.resolve(pid) instanceof Process;
    }

    static boolean hasLocal(Atom name) {
        PidOrPortId id = ProcessRegistry.instance().get(name);
        return id != null && ProcessRegistry.resolve(id) instanceof Process;
    }

    static void deliverLocal(PID pid, Term message) {
        ProcessOrPort process = ProcessRegistry.resolve(pid);
        if (process instanceof Process) {
            ((Process) process).send(message);
        }
    }

    static void deliverLocal(Atom name, Term message) {
        PidOrPortId id = ProcessRegistry.instance().get(name);
        if (id == null) {
            return;
        }
        ProcessOrPort process = ProcessRegistry.resolve(id);
        if (process instanceof Process) {
            ((Process) process).send(message);
        }
    }

    public static void disconnect(Atom node) {
        DistConnection connection = CONNECTIONS.remove(node);
        if (connection != null) {
            connection.close(DISCONNECT);
            fail(node);
            if (!isConnected(node)) {
                notifyNodeDown(connection, DISCONNECT);
            }
        }
    }

    public static void disconnectAll() {
        for (Atom node : new ArrayList<>(CONNECTIONS.keySet())) {
            disconnect(node);
        }
    }

    /**
     * Called by a connection when its reader thread terminates.
     */
    static void remove(DistConnection connection) {
        Atom node = connection.peer();
        if (CONNECTIONS.remove(node, connection)) {
            fail(node);
            // A replacement connection keeps the node up; otherwise the
            // node is down for every monitor_node/monitor_nodes subscriber.
            if (!isConnected(node)) {
                notifyNodeDown(connection, connection.closeReason());
                // An explicit disconnect is a decision, not a loss: only a
                // connection that broke is retried.
                if (!DISCONNECT.equals(connection.closeReason())) {
                    DistReconnect.peerLost(node);
                }
            }
        }
    }

    private static void notifyNodeUp(DistConnection connection) {
        for (ProcessOrPort process : ProcessRegistry.all()) {
            if (process instanceof Process) {
                ((Process) process).nodeUp(connection.peer(), connection.isHidden(),
                    connection.connectionId());
            }
        }
    }

    private static void notifyNodeDown(DistConnection connection, Term reason) {
        for (ProcessOrPort process : ProcessRegistry.all()) {
            if (process instanceof Process) {
                ((Process) process).nodeDown(connection.peer(), connection.isHidden(),
                    connection.connectionId(), reason);
            }
        }
    }

    /**
     * A node is no longer reachable: every link to it receives
     * noconnection and every monitor reports DOWN with noconnection, as in
     * Erlang/OTP. A replacement connection to the same node keeps the
     * signals alive.
     */
    private static void fail(Atom node) {
        DistConnection stillOpen = CONNECTIONS.get(node);
        if (stillOpen != null && stillOpen.isOpen()) {
            return;
        }
        SpawnRequests.connectionLost(node);
        for (ProcessOrPort process : ProcessRegistry.all()) {
            if (process instanceof Process) {
                ((Process) process).connectionLost(node);
            }
        }
    }

}
