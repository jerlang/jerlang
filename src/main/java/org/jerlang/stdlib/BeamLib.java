package org.jerlang.stdlib;

import static org.jerlang.kernel.File.enoent;
import static org.jerlang.kernel.File.eperm;

import java.io.DataInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.jerlang.erts.Erlang;
import org.jerlang.erts.erlang.Error;
import org.jerlang.stdlib.beam_lib.Chunk;
import org.jerlang.stdlib.beam_lib.ChunkId;
import org.jerlang.stdlib.beam_lib.MD5;
import org.jerlang.type.Atom;
import org.jerlang.type.Integer;
import org.jerlang.type.List;
import org.jerlang.type.Str;
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

    public static final String[] EXPORT = {
        "info/1",
        "md5/1"
    };

    public static final Atom beam_lib = Atom.of("beam_lib");
    public static final Atom error = Atom.of("error");
    public static final Atom file = Atom.of("file");
    public static final Atom file_error = Atom.of("file_error");
    public static final Atom invalid_beam_file = Atom.of("invalid_beam_file");
    public static final Atom not_a_beam_file = Atom.of("not_a_beam_file");

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
     * @throws IOException
     */
    public static Term info(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 1:
            return info_1(params.head().toStr());
        default:
            throw new Error("badarg");
        }
    }

    static Term info_1(Str filename_term) {
        File file = new File(filename_term.string());
        Term result = new List();

        if (!file.exists()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        }

        if (!file.canRead()) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, eperm));
        }

        try (FileInputStream fileInputStream = new FileInputStream(file)) {
            result = do_info(filename_term, new FileInputStream(file));
            if (result instanceof List) {
                result = new List(Tuple.of(Atom.of("file"), filename_term), (List) result);
            }
        } catch (FileNotFoundException fileNotFoundException) {
            return Tuple.of(error, beam_lib, Tuple.of(file_error, filename_term, enoent));
        } catch (EOFException eofException) {
            return Tuple.of(error, beam_lib, Tuple.of(invalid_beam_file, filename_term, Integer.of(0)));
        } catch (IOException ioException) {
            System.err.println("IOException: " + ioException);
        }

        return result;
    }

    private static Term do_info(Term filename, InputStream inputStream) throws IOException {
        List chunks = List.nil;
        DataInputStream dis = new DataInputStream(inputStream);
        if (dis.readInt() != 0x464f5231) { // "FOR1"
            return Tuple.of(not_a_beam_file, filename);
        }
        int length = dis.readInt();
        if (dis.readInt() != 0x4245414d) { // "BEAM"
            return Tuple.of(not_a_beam_file, filename);
        }
        int offset = 20;
        while (offset < length) {
            Chunk chunk = new Chunk(ChunkId.of(dis.readInt()), offset, dis.readInt());
            chunks = new List(chunk.asTuple(), chunks);
            offset += 8 + ((chunk.length() + 3) & ~3);
            dis.skipBytes((chunk.length() + 3) & ~3);
        }
        return new List(Tuple.of(Atom.of("chunks"), Lists.reverse_1(chunks)));
    }

    public static Term md5(List params) {
        switch (Erlang.length_1(params).toInt()) {
        case 1:
            return md5_1(params.head().toStr());
        default:
            throw new org.jerlang.erts.erlang.Error("badarg");
        }
    }

    static Term md5_1(Str filename) {
        return MD5.md5(filename);
    }

}
