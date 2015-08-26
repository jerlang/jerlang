package org.jerlang;

import org.jerlang.vm.VirtualMachine;
import org.junit.After;
import org.junit.Before;

public abstract class AbstractVirtualMachineTest {

    @Before
    public void startVirtualMachine() {
        VirtualMachine.instance().start();
    }

    @After
    public void stopVirtualMachine() {
        VirtualMachine.instance().shutdown();
    }

}
