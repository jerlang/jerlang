package org.jerlang.erts;

import java.util.HashMap;

import org.jerlang.type.Atom;
import org.jerlang.type.List;

/**
 * The runtime holds the global states of this node.
 */
public class Runtime {

    private static final HashMap<Atom, List> userFlags = new HashMap<>();

    public static HashMap<Atom, List> userFlags() {
        return userFlags;
    }

}
