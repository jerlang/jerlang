package org.jerlang.stdlib.shell;

import java.io.IOException;

import org.jerlang.FunctionSignature;
import org.jerlang.Module;
import org.jerlang.ModuleRegistry;
import org.jerlang.erts.Erlang;
import org.jerlang.erts.Init;
import org.jerlang.erts.erlang.Error;
import org.jerlang.exception.ThrowException;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

import jline.console.ConsoleReader;

public class ShellStart {

    private ShellStart() {
    }

    public static Term dispatch(List params) {
        switch (params.length()) {
        case 0:
            start_0();
            return null;
        default:
            throw new Error("badarg");
        }
    }

    public static void start_0() {
        try {
            ConsoleReader console = new ConsoleReader();
            console.setPrompt("> ");
            while (true) {
                try {
                    String line = console.readLine();
                    switch (line) {
                    case "init:stop().":
                        console.shutdown();
                        Init.stop();
                        break;
                    default:
                        if (!simple_call(line)) {
                            throw new Error("Unsupported operation");
                        }
                    }
                } catch (Error error) {
                    System.err.println(error);
                }
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

    // For PoC: try to parse line as a simple "m:f()." call
    private static boolean simple_call(String line) {
        if (!line.endsWith("().")) {
            return false;
        }
        line = line.substring(0, line.length() - 3);
        String[] token = line.split(":");
        if (token.length != 2) {
            return false;
        }
        Module m = ModuleRegistry.get(Atom.of(token[0]));
        if (m == null) {
            System.out.println("no module");
            return false;
        }
        FunctionSignature f = new FunctionSignature(m, token[1], 0);
        try {
            Erlang.display(m.apply(f, List.nil));
            return true;
        } catch (Error e) {
            System.out.println("error");
            e.printStackTrace();
            return false;
        } catch (ThrowException e) {
            e.printStackTrace();
            return false;
        }
    }

}
