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

# target/classes alone is not enough: the module loader unpacks compressed
# .beam chunks through jzlib, which is a dependency, so an example run against
# the bare classes dies with NoClassDefFoundError before the first call.
mvn -B -q -f "$repo/pom.xml" dependency:build-classpath \
    -Dmdep.outputFile="$work/classpath.txt" \
    -DincludeScope=runtime
deps="$(cat "$work/classpath.txt")"

# javac and java take the platform path separator, which on a Windows build is
# a semicolon: a drive letter already contains a colon, so joining with ":"
# splits inside the drive name and every package on the classpath disappears.
# Maven wrote the dependency list with that same separator, so it is reused.
# The classes directory also has to be in native form, because a mixed list of
# POSIX and Windows paths is not a path argument that the shell translates.
case "$(uname -s)" in
    CYGWIN*|MINGW*|MSYS*) sep=';' ;;
    *) sep=':' ;;
esac
if [ "$sep" = ';' ] && command -v cygpath >/dev/null 2>&1; then
    classes="$(cygpath -w "$classes")"
fi
classpath="$classes$sep$deps"

javac -cp "$classpath" -d "$work" \
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
java -cp "$classpath$sep." -Djerlang.otp.dir="$otp_root" GenServerDemo
echo "--- Erlang calls Java ---"
java -cp "$classpath$sep." ErlangCallsJavaDemo
