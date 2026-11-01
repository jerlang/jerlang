package org.jerlang.erts.dist;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.jerlang.type.Atom;
import org.junit.After;
import org.junit.Test;

/**
 * The reconnection policy from docs/operations.md: a lost peer is retried with
 * a doubling wait, a live connection forgets the retries, and shutting the node
 * down stops them.
 */
public class DistReconnectTest {

    @After
    public void stop() {
        DistReconnect.shutdown();
    }

    @Test
    public void theWaitDoublesAndStopsAtTheCap() {
        assertEquals(100, DistReconnect.delayMillis(1));
        assertEquals(200, DistReconnect.delayMillis(2));
        assertEquals(400, DistReconnect.delayMillis(3));
        assertEquals(5000, DistReconnect.delayMillis(20));
    }

    @Test
    public void aLostPeerIsRetriedUntilTheNodeShutsDown() {
        DistReconnect.start();
        Atom peer = Atom.of("peer@example");

        DistReconnect.peerLost(peer);
        assertTrue(DistReconnect.isRetrying(peer));

        DistReconnect.shutdown();
        assertFalse(DistReconnect.isRetrying(peer));
    }

    @Test
    public void aLiveConnectionForgetsItsRetries() {
        DistReconnect.start();
        Atom peer = Atom.of("peer@example");

        DistReconnect.peerLost(peer);
        DistReconnect.connected(peer);

        assertFalse(DistReconnect.isRetrying(peer));
    }

}
