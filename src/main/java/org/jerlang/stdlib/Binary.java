package org.jerlang.stdlib;

import org.jerlang.stdlib.binary.BinaryAt;
import org.jerlang.stdlib.binary.BinaryBinToList;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Tuple;

/**
 * = binary
 *
 * == Summary
 *
 * Library for handling binary data
 *
 * == Description
 *
 * This module contains functions for manipulating byte-oriented binaries.
 * Although the majority of functions could be implemented using bit-syntax,
 * the functions in this library are highly optimized and are expected to
 * either execute faster or consume less memory (or both) than a counterpart
 * written in pure Erlang.
 *
 * The module is implemented according to EEP 31.
 *
 * Based on:
 * http://www.erlang.org/doc/man/binary.html
 */
public class Binary {

    public Integer at(org.jerlang.type.Binary key, Integer pos) {
        return BinaryAt.at_2(key, pos);
    }

    public List bin_to_list(org.jerlang.type.Binary subject) {
        return BinaryBinToList.bin_to_list_1(subject);
    }

    public List bin_to_list(org.jerlang.type.Binary subject, Tuple part) {
        return BinaryBinToList.bin_to_list_2(subject, part);
    }

    public List bin_to_list(org.jerlang.type.Binary subject, Integer pos, Integer len) {
        return BinaryBinToList.bin_to_list_3(subject, pos, len);
    }

}
