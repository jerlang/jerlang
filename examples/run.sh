#!/usr/bin/env bash
#
# Compiles and runs both examples. Needs a JDK and Erlang/OTP on the PATH: the
# first example runs OTP's own gen_server, so it is not a JVM-only demo.
#
# Run from anywhere; the script finds the repository root itself.
set -euo pipefail

repo="$(cd "$(dirname "$0")/.." && pwd)"
classes="$repo/target/classes"
work="$repo/target/examples"

if [ ! -d "$classes" ]; then
    echo "compile the runtime first: mvn -B -DskipTests compile" >&2
    exit 2
fi

otp_root="$(erl -noshell -eval 'io:format("~s", [code:root_dir()]), halt().')"
echo "OTP root: $otp_root"

rm -rf "$work"
mkdir -p "$work"

javac -cp "$classes" -d "$work" \
    "$repo/examples/gen-server/GenServerDemo.java" \
    "$repo/examples/java-service/Service.java" \
    "$repo/examples/java-service/ErlangCallsJavaDemo.java"

erlc -o "$work" \
    "$repo/examples/gen-server/counter.erl" \
    "$repo/examples/java-service/service.erl"

# The module loader looks for a .beam in the working directory, so the
# examples run from the directory the modules were compiled into.
cd "$work"
echo "--- Java calls Erlang ---"
java -cp "$classes:." -Djerlang.otp.dir="$otp_root" GenServerDemo
echo "--- Erlang calls Java ---"
java -cp "$classes:." ErlangCallsJavaDemo
