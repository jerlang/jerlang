package org.jerlang;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.nio.file.Files;

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
            LiteralTableChunk literalTableChunk = null;
            LocalFunctionTableChunk localFunctionTableChunk = null;
            StringTableChunk stringTableChunk = null;
            byte[] bytes = Files.readAllBytes(new File(filename).toPath());
            Tuple chunksTuple = get_chunks_tuple(info);
            List chunkList = chunksTuple.element(2).toList();
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
                    codeChunk = new CodeChunkReader(chunk, dis, atomChunk).read();
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
                    // not supported
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
            BeamData bd = new BeamData(
                abstractSyntaxTreeChunk,
                atomChunk,
                attributeChunk,
                codeChunk,
                compileInfoChunk,
                exportTableChunk,
                functionTableChunk,
                importTableChunk,
                literalTableChunk,
                localFunctionTableChunk,
                stringTableChunk);
            System.out.println(bd);
            return bd;
        } catch (Throwable e) {
            e.printStackTrace();
            return null;
        }
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

    public static void main(String[] args) {
        ModuleLoader.load(Atom.of("hello_world"));
    }

}
