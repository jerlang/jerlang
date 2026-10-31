package org.jerlang.jvm;

import org.jerlang.AbstractVirtualMachineTest;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.stdlib.Java;
import org.jerlang.type.Atom;
import org.junit.Before;

/**
 * The java module is not a built-in any more: a node only has it when it asks
 * for reflection, so a test that calls java:* registers it itself.
 */
public abstract class AbstractJavaInteropTest extends AbstractVirtualMachineTest {

    @Before
    public void registerJavaModule() {
        Module module = new Module(Java.class, Atom.of("java"));
        module.export();
        ModuleRegistry.register(module);
    }

}
