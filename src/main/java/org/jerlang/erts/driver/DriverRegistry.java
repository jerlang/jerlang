package org.jerlang.erts.driver;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A simple registry for drivers.
 */
public class DriverRegistry {

    private static final Map<String, Driver> DRIVERS = new ConcurrentHashMap<>();

    private DriverRegistry() {
    }

    public static Driver find(String driverName) {
        return DRIVERS.get(driverName);
    }

    public static void register(Driver driver) {
        DRIVERS.put(driver.toString(), driver);
    }

}
