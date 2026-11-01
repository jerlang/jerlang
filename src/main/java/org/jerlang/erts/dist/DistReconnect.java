package org.jerlang.erts.dist;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.jerlang.type.Atom;

/**
 * Retries connections to nodes that went away.
 *
 * A lost connection is not the same as a lost link: Erlang/OTP reports
 * `noconnection` to every link and monitor on the peer and leaves
 * reconnection to the code that needs the node. A service, however, usually
 * wants its peer back, so this class keeps retrying a peer that was connected
 * and went away, until it answers again or the attempts run out. Talking to
 * the peer after that, or calling {@code net_kernel:connect_node/1}, connects
 * on demand as it always did.
 *
 * The wait between attempts starts at {@value #FIRST_DELAY_MILLIS} ms and
 * doubles up to {@value #MAX_DELAY_MILLIS} ms, for at most
 * {@value #MAX_ATTEMPTS} attempts: roughly half a minute. Reconnecting fires
 * `net_kernel:monitor_nodes/1` events like any other connection; links and
 * monitors that the peer's crash reported as `noconnection` are not
 * re-established, because they belong to the processes that saw them fail.
 *
 * Retries are on by default and can be turned off with
 * {@code -Djerlang.dist.reconnect=false}.
 */
final class DistReconnect {

    private static final int MAX_ATTEMPTS = 12;
    private static final long FIRST_DELAY_MILLIS = 100;
    private static final long MAX_DELAY_MILLIS = 5000;

    private static final ConcurrentHashMap<Atom, AtomicInteger> ATTEMPTS =
        new ConcurrentHashMap<>();

    private static volatile ScheduledExecutorService executor;

    private DistReconnect() {
    }

    static synchronized void start() {
        if (executor != null) {
            return;
        }
        executor = Executors.newSingleThreadScheduledExecutor(runnable -> {
            Thread thread = new Thread(runnable, "jerlang-dist-reconnect");
            thread.setDaemon(true);
            return thread;
        });
    }

    static synchronized void shutdown() {
        ScheduledExecutorService current = executor;
        executor = null;
        if (current != null) {
            current.shutdownNow();
        }
        ATTEMPTS.clear();
    }

    /**
     * A connection to this node is live again: forget its retries.
     */
    static void connected(Atom peer) {
        if (peer != null) {
            ATTEMPTS.remove(peer);
        }
    }

    /**
     * Whether this node still has retries scheduled.
     */
    static boolean isRetrying(Atom peer) {
        return ATTEMPTS.containsKey(peer);
    }

    /**
     * A connection to this node was lost: retry it until it answers.
     */
    static void peerLost(Atom peer) {
        ScheduledExecutorService current = executor;
        if (peer == null || current == null) {
            return;
        }
        if (!Boolean.parseBoolean(System.getProperty("jerlang.dist.reconnect", "true"))) {
            return;
        }
        AtomicInteger counter = ATTEMPTS.computeIfAbsent(peer, key -> new AtomicInteger());
        int attempt = counter.incrementAndGet();
        if (attempt > MAX_ATTEMPTS) {
            ATTEMPTS.remove(peer);
            return;
        }
        try {
            current.schedule(() -> retry(peer), delayMillis(attempt), TimeUnit.MILLISECONDS);
        } catch (RejectedExecutionException e) {
            ATTEMPTS.remove(peer);
        }
    }

    private static void retry(Atom peer) {
        if (executor == null) {
            return;
        }
        try {
            if (Distribution.connect(peer) && Distribution.isConnected(peer)) {
                ATTEMPTS.remove(peer);
                return;
            }
        } catch (RuntimeException e) {
            // A failed attempt is retried like any other.
        }
        peerLost(peer);
    }

    /**
     * The wait before attempt number {@code attempt}, counting from one.
     */
    static long delayMillis(int attempt) {
        if (attempt < 1) {
            return FIRST_DELAY_MILLIS;
        }
        long delay = FIRST_DELAY_MILLIS << Math.min(attempt - 1, 16);
        return Math.min(delay, MAX_DELAY_MILLIS);
    }

}
