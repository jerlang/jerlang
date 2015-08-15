package org.jerlang.stdlib.maps;

import org.jerlang.erts.erlang.Error;
import org.jerlang.type.List;
import org.jerlang.type.Map;
import org.jerlang.type.Term;

public class MapsNew {

    private MapsNew() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            return new_0();
        default:
            throw new Error("badarg");
        }
    }

    /**
     * http://www.erlang.org/doc/man/maps.html#new-0
     */
    public static Map new_0() {
        return new Map();
    }

}
