package org.jerlang;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;
import java.util.Comparator;
import java.util.TreeMap;

import org.jerlang.erts.Erlang;
import org.jerlang.exception.ThrowException;
import org.jerlang.stdlib.BeamLib;
import org.jerlang.stdlib.beam_lib.AbstractSyntaxTreeChunk;
import org.jerlang.stdlib.beam_lib.AbstractSyntaxTreeChunkReader;
import org.jerlang.stdlib.beam_lib.AtomChunk;
import org.jerlang.stdlib.beam_lib.AtomChunkReader;
import org.jerlang.stdlib.beam_lib.AttributeChunk;
import org.jerlang.stdlib.beam_lib.AttributeChunkReader;
import org.jerlang.stdlib.beam_lib.BeamData;
import org.jerlang.stdlib.beam_lib.Chunk;
import org.jerlang.stdlib.beam_lib.ChunkId;
import org.jerlang.stdlib.beam_lib.CodeChunk;
import org.jerlang.stdlib.beam_lib.CodeChunkReader;
import org.jerlang.stdlib.beam_lib.CompileInfoChunk;
import org.jerlang.stdlib.beam_lib.CompileInfoChunkReader;
import org.jerlang.stdlib.beam_lib.ExportTableChunk;
import org.jerlang.stdlib.beam_lib.ExportTableChunkReader;
import org.jerlang.stdlib.beam_lib.FunctionTableChunk;
import org.jerlang.stdlib.beam_lib.FunctionTableChunkReader;
import org.jerlang.stdlib.beam_lib.ImportTableChunk;
import org.jerlang.stdlib.beam_lib.ImportTableChunkReader;
import org.jerlang.stdlib.beam_lib.LineTableChunk;
import org.jerlang.stdlib.beam_lib.LineTableChunkReader;
import org.jerlang.stdlib.beam_lib.LiteralTableChunk;
import org.jerlang.stdlib.beam_lib.LiteralTableChunkReader;
import org.jerlang.stdlib.beam_lib.LocalFunctionTableChunk;
import org.jerlang.stdlib.beam_lib.LocalFunctionTableChunkReader;
import org.jerlang.stdlib.beam_lib.StringTableChunk;
import org.jerlang.stdlib.beam_lib.StringTableChunkReader;
import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Str;
import org.jerlang.type.Term;
import org.jerlang.type.Tuple;
import org.jerlang.util.ByteUtil;

/**
 * The ModuleLoader loads a BEAM file into a BeamData object and
 * registers the loaded module at ModuleRegistry.
 */
public class ModuleLoader {

    /**
     * Try to load the module from file system
     */
    public static void load(Atom module) {
        String filename = module.toString() + ".beam";
        Term info = BeamLib.info(Str.of(filename));
        if (!(info instanceof List)) {
            System.err.println("Can not load module: " + info);
            return;
        }
        Module m = new Module(loadBeamData(filename, info.toList()), module);
        ModuleRegistry.register(m);
        m.export();
    }

    private static BeamData loadBeamData(String filename, List info) {
        try {
            AbstractSyntaxTreeChunk abstractSyntaxTreeChunk = null;
            AtomChunk atomChunk = null;
            AttributeChunk attributeChunk = null;
            CodeChunk codeChunk = null;
            CompileInfoChunk compileInfoChunk = null;
            ExportTableChunk exportTableChunk = null;
            FunctionTableChunk functionTableChunk = null;
            ImportTableChunk importTableChunk = null;
            LineTableChunk lineTableChunk = null;
            LiteralTableChunk literalTableChunk = null;
            LocalFunctionTableChunk localFunctionTableChunk = null;
            StringTableChunk stringTableChunk = null;
            byte[] bytes = ByteUtil.maybe_decompress(Files.readAllBytes(new File(filename).toPath()));
            Tuple chunksTuple = get_chunks_tuple(info);
            List chunkList = sort_chunk_list(chunksTuple.element(2).toList());
            while (chunkList.length() > 0) {
                Tuple chunkTuple = chunkList.head().toTuple();
                int offset = chunkTuple.element(2).toInteger().toInt();
                int length = chunkTuple.element(3).toInteger().toInt();
                ChunkId chunkId = ChunkId.of(chunkTuple.element(1).toStr().string());
                Chunk chunk = new Chunk(chunkId, offset, length);
                DataInputStream dis = new DataInputStream(new ByteArrayInputStream(bytes));
                dis.skipBytes(offset);
                switch (chunkId) {
                case ABST:
                    abstractSyntaxTreeChunk = new AbstractSyntaxTreeChunkReader(chunk, dis).read();
                    break;
                case ATOM:
                    atomChunk = new AtomChunkReader(chunk, dis).read();
                    break;
                case ATTR:
                    attributeChunk = new AttributeChunkReader(chunk, dis).read();
                    break;
                case CINF:
                    compileInfoChunk = new CompileInfoChunkReader(chunk, dis).read();
                    break;
                case CODE:
                    codeChunk = new CodeChunkReader(chunk, dis, atomChunk, literalTableChunk).read();
                    break;
                case EXPT:
                    exportTableChunk = new ExportTableChunkReader(chunk, dis, atomChunk).read();
                    break;
                case FUNT:
                    functionTableChunk = new FunctionTableChunkReader(chunk, dis, atomChunk).read();
                    break;
                case IMPT:
                    importTableChunk = new ImportTableChunkReader(chunk, dis, atomChunk).read();
                    break;
                case LINE:
                    lineTableChunk = new LineTableChunkReader(chunk, dis, atomChunk).read();
                    break;
                case LITT:
                    literalTableChunk = new LiteralTableChunkReader(chunk, dis).read();
                    break;
                case LOCT:
                    localFunctionTableChunk = new LocalFunctionTableChunkReader(chunk, dis, atomChunk).read();
                    break;
                case STRT:
                    stringTableChunk = new StringTableChunkReader(chunk, dis).read();
                    break;
                default:
                    break;
                }
                chunkList = chunkList.tail();
            }
            BeamData beamData = new BeamData(
                abstractSyntaxTreeChunk,
                atomChunk,
                attributeChunk,
                codeChunk,
                compileInfoChunk,
                exportTableChunk,
                functionTableChunk,
                importTableChunk,
                lineTableChunk,
                literalTableChunk,
                localFunctionTableChunk,
                stringTableChunk);
            // System.out.println(beamData);
            return beamData;
        } catch (Throwable e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * We need to sort the list of chunks so that chunks
     * without dependencies (e.g. the atom chunk) are loaded first.
     * Otherwise, loading of chunks depending on such chunks,
     * like the code chunk, is not possible.
     */
    private static List sort_chunk_list(List chunkList) {
        TreeMap<ChunkId, Tuple> chunks = new TreeMap<>(new Comparator<ChunkId>() {

            @Override
            public int compare(ChunkId a, ChunkId b) {
                return -Integer.compare(a.sortOrder(), b.sortOrder());
            }

        });
        while (chunkList.length() > 0) {
            Tuple chunkTuple = chunkList.head().toTuple();
            ChunkId chunkId = ChunkId.of(chunkTuple.element(1).toStr().string());
            chunks.put(chunkId, chunkTuple);
            chunkList = chunkList.tail();
        }
        List result = List.of(chunks.values());
        return result;
    }

    private static Tuple get_chunks_tuple(List info) {
        Atom chunks = Atom.of("chunks");
        while (info.length() > 0) {
            Tuple tuple = info.head().toTuple();
            if (chunks.equals(tuple.element(1))) {
                return tuple;
            }
            info = info.tail();
        }
        return null;
    }

    public static void main(String[] args) throws ThrowException {
        Erlang.apply(Atom.of("fun_test"), Atom.of("t1"), List.nil);
    }

}
