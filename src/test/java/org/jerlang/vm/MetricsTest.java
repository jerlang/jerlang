package org.jerlang.vm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.jerlang.ModuleRegistry;
import org.jerlang.api.Node;
import org.jerlang.api.NodeOptions;
import org.jerlang.type.Atom;
import org.junit.After;
import org.junit.Test;

/**
 * The metrics surface from docs/operations.md: a snapshot reports the pool it
 * runs on, the work that is alive shows up in it, and a closed node reports
 * nothing.
 */
public class MetricsTest {

    @After
    public void stop() {
        VirtualMachine.instance().shutdown();
        ModuleRegistry.reset();
    }

    @Test
    public void aRunningNodeReportsItsPool() {
        try (Node node = Node.start(NodeOptions.builder().schedulers(3).build())) {
            Metrics metrics = node.metrics();
            assertEquals(3, metrics.schedulerCount());
            assertTrue(metrics.uptimeMillis() >= 0);
            assertTrue(metrics.utilisation() >= 0.0);
            assertTrue(metrics.utilisation() <= 1.0);
            assertEquals(0, metrics.ports());

            String text = metrics.toText();
            assertTrue(text, text.contains("jerlang_schedulers 3\n"));
            assertEquals(3, occurrences(text, "jerlang_scheduler_queue{index="));
            assertEquals(3, occurrences(text, "jerlang_scheduler_utilisation{index="));
        }
    }

    @Test
    public void aClosedNodeReportsNothing() {
        Node node = Node.start(NodeOptions.builder().schedulers(2).build());
        node.close();

        Metrics metrics = Metrics.snapshot();
        assertEquals(0, metrics.uptimeMillis());
        assertEquals(0, metrics.processes());
        assertEquals(0, metrics.ports());
        assertEquals(0, metrics.mailboxes());
    }

    @Test
    public void anIdleNodeSpendsItsTimeParked() throws Exception {
        try (Node node = Node.start(NodeOptions.builder().schedulers(2).build())) {
            Thread.sleep(120);

            Metrics metrics = node.metrics();
            assertTrue("the node has been up: " + metrics, metrics.uptimeMillis() >= 100);
            assertTrue("nothing is running: " + metrics, metrics.processes() == 0);
            assertTrue("no messages are waiting: " + metrics, metrics.mailboxes() == 0);
            assertTrue("an idle scheduler is not fully busy: " + metrics,
                metrics.utilisation() < 1.0);
        }
    }

    private static int occurrences(String text, String needle) {
        int count = 0;
        int index = text.indexOf(needle);
        while (index >= 0) {
            count++;
            index = text.indexOf(needle, index + needle.length());
        }
        return count;
    }

}
