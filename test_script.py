import os
import subprocess
import sys

def run_cmd(cmd, timeout_val=60):
    try:
        res = subprocess.run(cmd, shell=True, capture_output=True, timeout=timeout_val)
        return res.returncode, res.stdout, res.stderr
    except subprocess.TimeoutExpired:
        return -1, b'', b'Timeout'

def run_perf(cmd, timeout_val=60):
    perf_cmd = f"perf stat -x ';' -e task-clock {cmd}"
    try:
        res = subprocess.run(perf_cmd, shell=True, capture_output=True, timeout=timeout_val, text=True)
        for line in res.stderr.splitlines():
            if 'task-clock' in line:
                parts = line.split(';')
                try:
                    return res.returncode, float(parts[0]) / 1000.0
                except ValueError:
                    pass
        return res.returncode, -1.0
    except subprocess.TimeoutExpired:
        return -1, -1.0

def main():
    print("Gathering benchmark files...")
    test_files_raw = subprocess.run('find generated/bench2/benchmarks -name "*.cml"', shell=True, capture_output=True, text=True).stdout.strip().split('\n')
    test_files = [f for f in test_files_raw if f]

    exclude_list = ["smith-normal-form.cml", "pi-digits.cml"]
    test_files = [f for f in test_files if not any(x in f for x in exclude_list)]

    original_cake = "./generated/bench2/compilers/cakeml/3044/cake-x64-64/cake"
    opt_cake = "./generated/cake64_opt"

    print(f"{'Test':<20} | {'Orig Avg (s)':<12} | {'Opt Avg (s)':<12} | {'Speedup':<8} | {'Orig Size':<10} | {'Opt Size':<10} | {'Correct':<7}")
    print("-" * 92)

    for cml_file in test_files:
        name = os.path.basename(cml_file).replace('.cml', '')

        orig_s = f"/tmp/{name}_orig.S"
        opt_s = f"/tmp/{name}_opt2.S"
        orig_exe = f"/tmp/{name}_orig.exe"
        opt_exe = f"/tmp/{name}_opt2.exe"

        rc1, _, _ = run_cmd(f"{original_cake} < {cml_file} > {orig_s}", timeout_val=60)
        rc2, _, _ = run_cmd(f"{opt_cake} < {cml_file} > {opt_s}", timeout_val=60)

        if rc1 != 0 or rc2 != 0:
            print(f"{name:<20} | {'CML_FAIL':<12} | {'-':<12} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<7}")
            continue

        rc3, _, _ = run_cmd(f"gcc basis_ffi.c {orig_s} -lm -o {orig_exe}")
        rc4, _, _ = run_cmd(f"gcc basis_ffi.c {opt_s} -lm -o {opt_exe}")

        if rc3 != 0 or rc4 != 0:
            print(f"{name:<20} | {'GCC_FAIL':<12} | {'-':<12} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<7}")
            continue

        run_cmd(f"strip {orig_exe}")
        run_cmd(f"strip {opt_exe}")

        orig_size = os.path.getsize(orig_exe)
        opt_size = os.path.getsize(opt_exe)

        rc_test, out1, _ = run_cmd(orig_exe, timeout_val=30)
        rc_test2, out2, _ = run_cmd(opt_exe, timeout_val=30)

        if rc_test != 0 or rc_test2 != 0:
            print(f"{name:<20} | {'RUN_FAIL':<12} | {'-':<12} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<7}")
            continue

        correct = (out1 == out2) and (rc_test == rc_test2)
        correct_str = "YES" if correct else "NO"

        # Warmup (1 run)
        run_cmd(orig_exe, timeout_val=30)
        run_cmd(opt_exe, timeout_val=30)

        # Actual Benchmarking (2 runs, interweaved to prevent biasness: AB, BA)

        # Run 1: Orig then Opt
        rc_o1, t_o1 = run_perf(orig_exe, timeout_val=30)
        rc_op1, t_op1 = run_perf(opt_exe, timeout_val=30)

        # Run 2: Opt then Orig
        rc_op2, t_op2 = run_perf(opt_exe, timeout_val=30)
        rc_o2, t_o2 = run_perf(orig_exe, timeout_val=30)

        if rc_o1 != 0 or rc_op1 != 0 or rc_o2 != 0 or rc_op2 != 0 or t_o1 < 0 or t_op1 < 0 or t_o2 < 0 or t_op2 < 0:
            print(f"{name:<20} | {'RUN_ERR/TIME':<12} | {'-':<12} | {'-':<8} | {'-':<10} | {'-':<10} | {'-':<7}")
            continue

        orig_times = [t_o1, t_o2]
        opt_times = [t_op1, t_op2]

        avg_orig = sum(orig_times) / len(orig_times)
        avg_opt = sum(opt_times) / len(opt_times)

        speedup = avg_orig / avg_opt if avg_opt > 0 else 0

        print(f"{name:<20} | {avg_orig:<12.5f} | {avg_opt:<12.5f} | {speedup:<8.2f} | {orig_size:<10} | {opt_size:<10} | {correct_str:<7}")

if __name__ == "__main__":
    main()
