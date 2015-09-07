package org.jerlang.erts.erlang;

import org.jerlang.type.Atom;
import org.jerlang.type.List;
import org.jerlang.type.Term;

public class ErlangSystemInfo {

    public enum Terms {
        allocated_areas,
        allocator,
        allocator_sizes,
        alloc_util_allocators,
        build_type,
        check_io,
        compat_rel,
        cpu_topology,
        creation,
        c_compiler_used,
        debug_compiled,
        delayed_node_table_gc,
        dirty_cpu_schedulers,
        dirty_cpu_schedulers_online,
        dirty_io_schedulers,
        dist,
        dist_buf_busy_limit,
        dist_ctrl,
        driver_version,
        dynamic_trace,
        dynamic_trace_probes,
        eager_check_io,
        elib_malloc,
        end_time,
        ets_limit,
        fullsweep_after,
        garbage_collection,
        heap_sizes,
        heap_type,
        info,
        kernel_poll,
        loaded,
        logical_processors,
        logical_processors_available,
        logical_processors_online,
        machine,
        min_bin_vheap_size,
        min_heap_size,
        modified_timing_level,
        multi_scheduling,
        multi_scheduling_blockers,
        nif_version,
        os_monotonic_time_source,
        os_system_time_source,
        otp_release,
        port_count,
        port_limit,
        port_parallelism,
        process_count,
        process_limit,
        procs,
        schedulers,
        schedulers_online,
        scheduler_bindings,
        scheduler_bind_type,
        scheduler_id,
        smp_support,
        start_time,
        system_architecture,
        system_version,
        threads,
        thread_pool_size,
        time_correction,
        time_offset,
        time_warp_mode,
        tolerant_timeofday,
        trace_control_word,
        update_cpu_info,
        version,
        wordsize;

        private final Atom atom;

        private Terms() {
            this.atom = Atom.of(name());
        }

        public Atom atom() {
            return atom;
        }
    }

    private ErlangSystemInfo() {
    }

    public static Term dispatch(List params) throws Error {
        switch (params.length()) {
        case 1:
            return system_info_1(params.head());
        default:
            throw new Error("badarg");
        }
    }

    /**
     * Returns various information about the allocators of the
     * current system (emulator) as specified by Item:
     *
     * http://www.erlang.org/doc/man/erlang.html#system_info-1
     */
    public static Term system_info_1(Term term) throws Error {
        return null;
    }

}
