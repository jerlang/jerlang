package org.jerlang.stdlib.beam_lib;

import static org.jerlang.kernel.file.File.enoent;
import static org.jerlang.kernel.file.File.eperm;

import java.io.File;

import org.jerlang.type.Atom;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;

/**
 * = beam_lib
 *
 * == MODULE
 *
 * http://www.erlang.org/doc/man/beam_lib.html[beam_lib]
 *
 * == MODULE SUMMARY
 *
 * An interface to the BEAM file format.
 *
 * == DESCRIPTION
 *
 * beam_lib provides an interface to files created by the BEAM compiler ("BEAM files").
 * The format used, a variant of "EA IFF 1985" Standard for Interchange Format Files,
 * divides data into chunks.
 *
 * Chunk data can be returned as binaries or as compound terms.
 * Compound terms are returned when chunks are referenced by names (atoms) rather than
 * identifiers (strings). The names recognized and the corresponding identifiers are:
 *
 * * abstract_code ("Abst")
 * * attributes ("Attr")
 * * compile_info ("CInf")
 * * exports ("ExpT")
 * * labeled_exports ("ExpT")
 * * imports ("ImpT")
 * * indexed_imports ("ImpT")
 * * locals ("LocT")
 * * labeled_locals ("LocT")
 * * atoms ("Atom")
 *
 * == DATA TYPES
 *
 * ----
 * beam() = module()
 *        | file:filename()
 *        | binary()
 * ----
 *
 * Each of the functions described below accept either the module name,
 * the filename, or a binary containing the beam module.
 *
 * ----
 * chunkdata() = {chunkid(), dataB()}
 *             | {abstract_code, abst_code()}
 *             | {attributes, [attrib_entry()]}
 *             | {compile_info, [compinfo_entry()]}
 *             | {exports, [{atom(), arity()}]}
 *             | {labeled_exports, [labeled_entry()]}
 *             | {imports, [mfa()]}
 *             | {indexed_imports,
 *                [{index(),
 *                  module(),
 *                  Function :: atom(),
 *                  arity()}]}
 *             | {locals, [{atom(), arity()}]}
 *             | {labeled_locals, [labeled_entry()]}
 *             | {atoms, [{integer(), atom()}]}
 * ----
 *
 * The list of attributes is sorted on Attribute (in attrib_entry()),
 * and each attribute name occurs once in the list.
 * The attribute values occur in the same order as in the file.
 * The lists of functions are also sorted.
 *
 * ----
 * chunkid() = nonempty_string()
 * ----
 *
 * "Abst" | "Attr" | "CInf" | "ExpT" | "ImpT" | "LocT" | "Atom"
 *
 * ----
 * dataB() = binary()
 * ----
 *
 * ----
 * abst_code() = {AbstVersion :: atom(), forms()}
 *             | no_abstract_code
 * ----
 * 
 * It is not checked that the forms conform to the abstract format indicated by
 * AbstVersion. no_abstract_code means that the "Abst" chunk is present, but empty.
 *
 * ----
 * forms() = [erl_parse:abstract_form()]
 * ----
 *
 * ----
 * compinfo_entry() = {InfoKey :: atom(), term()}
 * ----
 *
 * ----
 * attrib_entry() = {Attribute :: atom(), [AttributeValue :: term()]}
 * ----
 *
 * ----
 * labeled_entry() = {Function :: atom(), arity(), label()}
 * ----
 *
 * ----
 * index() = integer() >= 0
 * ----
 *
 * ----
 * label() = integer()
 * ----
 * 
 * ----
 * chunkref() = chunkname() | chunkid()
 * ----
 *
 * ----
 * chunkname() = abstract_code
 *            | attributes
 *            | compile_info
 *            | exports
 *            | labeled_exports
 *            | imports
 *            | indexed_imports
 *            | locals
 *            | labeled_locals
 *            | atoms
 * ----
 *
 * ----
 * chnk_rsn() = {unknown_chunk, file:filename(), atom()}
 *            | {key_missing_or_invalid,
 *               file:filename(),
 *               abstract_code}
 *            | info_rsn()
 * ----
 *
 * ----
 * info_rsn() = {chunk_too_big,
 *               file:filename(),
 *               chunkid(),
 *               ChunkSize :: integer() >= 0,
 *               FileSize :: integer() >= 0}
 *            | {invalid_beam_file,
 *               file:filename(),
 *               Position :: integer() >= 0}
 *            | {invalid_chunk, file:filename(), chunkid()}
 *            | {missing_chunk, file:filename(), chunkid()}
 *            | {not_a_beam_file, file:filename()}
 *            | {file_error, file:filename(), file:posix()}
 * ----
 */

public class BeamLib {

    public static final Atom beam_lib = new Atom("beam_lib");
    public static final Atom error = new Atom("error");
    public static final Atom file_error = new Atom("file_error");

    private BeamLib() {
    }

    /**
     * ----
     * info(Beam) -> [InfoPair] | {error, beam_lib, info_rsn()}
     * ----
     *
     * Types:
     * 
     * ----
     * Beam = beam()
     * ----
     *
     * ----
     * InfoPair = {file, Filename :: file:filename()}
     *          | {binary, Binary :: binary()}
     *          | {module, Module :: module()}
     *          | {chunks,
     *            [{ChunkId :: chunkid(),
     *              Pos :: integer() >= 0,
     *              Size :: integer() >= 0}]}
     * ----
     *
     * Returns a list containing some information about a BEAM file as tuples {Item, Info}:
     *
     * ----
     * {file, Filename} | {binary, Binary}
     * ----
     * The name (string) of the BEAM file, or the binary from which the information was extracted.
     *
     * ----
     * {module, Module}
     * ----
     * The name (atom) of the module.
     *
     * ----
     * {chunks, [{ChunkId, Pos, Size}]}
     * ----
     * For each chunk, the identifier (string) and the position and size of the chunk data, in bytes.
     */
    public static Term info(String filename) {
        File file = new File(filename);
        Term filename_term = Term.of(filename);

        if (!file.exists()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        }

        if (!file.canRead()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, eperm));
        }

        return null;
    }
}
