package org.jerlang.jvm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.jerlang.api.Node;
import org.jerlang.api.NodeOptions;
import org.jerlang.type.Integer;
import org.jerlang.vm.Metrics;
import org.junit.Test;

/**
 * The 0.9 exit test at the embedding boundary: a node serves calls under load
 * while it is scraped, no call is lost, and closing it leaves nothing behind.
 */
public class OperationsTest {

    @Test
    public void aLoadedNodeStaysScrapeableAndClosesClean() throws Exception {
        try (Node node = Node.start(NodeOptions.builder().schedulers(2).build())) {
            node.registerFunction("load_svc", "echo", 1, args -> args[0]);

            int threads = 4;
            int calls = 100;
            ExecutorService pool = Executors.newFixedThreadPool(threads);
            AtomicReference<Throwable> failure = new AtomicReference<>();
            CountDownLatch done = new CountDownLatch(threads);
            for (int thread = 0; thread < threads; thread++) {
                pool.execute(() -> {
                    try {
                        for (int call = 0; call < calls; call++) {
                            Integer reply = (Integer) node.apply("load_svc", "echo",
                                Integer.of(call));
                            if (!Integer.of(call).equals(reply)) {
                                throw new AssertionError("lost call " + call);
                            }
                        }
                    } catch (Throwable e) {
                        failure.compareAndSet(null, e);
                    } finally {
                        done.countDown();
                    }
                });
            }

            while (!done.await(1, TimeUnit.MILLISECONDS)) {
                Metrics metrics = node.metrics();
                assertEquals(2, metrics.schedulerCount());
                assertTrue(metrics.utilisation() >= 0.0 && metrics.utilisation() <= 1.0);
                assertTrue(metrics.processes() >= 0);
                assertTrue(metrics.toText().contains("jerlang_processes"));
            }

            pool.shutdown();
            assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS));
            assertNull(failure.get());
            assertEquals(0, node.metrics().mailboxes());
        }

        Metrics after = Metrics.snapshot();
        assertEquals(0, after.processes());
        assertEquals(0, after.ports());
        assertEquals(0, after.uptimeMillis());
    }

}
