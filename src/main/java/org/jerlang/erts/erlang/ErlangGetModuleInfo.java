package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

/**
 *
 */
public class ErlangGetModuleInfo {

    private ErlangGetModuleInfo() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 2:
            Atom module = params.head().toAtom();
            params = params.tail();
            Atom item = params.head().toAtom();
            return get_module_info_2(module, item);
        default:
            throw Error.badarg;
        }
    }

    public static Term get_module_info_2(Atom module, Atom item) {
        switch (item.toString()) {
        case "module":
            return module;
        case "md5":
        case "exports":
        case "functions":
        case "attributes":
        case "compile":
        case "native_addresses":
        case "native":
            // TODO
        default:
            throw new Error("badarg: " + module);
        }
    }

}
