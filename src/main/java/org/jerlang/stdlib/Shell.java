package org.jerlang.stdlib;

import java.io.IOException;

import jline.console.ConsoleReader;

import org.jerlang.erts.Init;
import org.jerlang.erts.erlang.Error;

/**
 * = shell
 *
 * The Erlang Shell
 *
 * http://www.erlang.org/doc/man/shell.html
 */
public class Shell {

    public static void start() {
        try {
            ConsoleReader console = new ConsoleReader();
            console.setPrompt("> ");
            while (true) {
                try {
                    switch (console.readLine()) {
                    case "init:stop().":
                        console.shutdown();
                        Init.stop();
                        break;
                    default:
                        throw new Error("Unsupported operation");
                    }
                } catch (Error error) {
                    System.err.println(error);
                }
            }
        } catch (IOException ioException) {
            ioException.printStackTrace();
        }
    }

}
