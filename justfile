os := if os() == "macos" { "osx" } else { os() }
arch := arch() + "-" + os
ghc-version := `ghc --version | awk '{print $8}'`
version := `cat AoC24.cabal | grep version | grep -v cabal | awk '{print $2}'`
build-path := join("dist-newstyle/build", arch, "ghc-" + ghc-version, "AoC24-" + version)

# show available just commands
default:
    @just --list --unsorted --list-heading $'Available commands:\n'

_do-build target opt *flags:
    cabal build -j -O{{ opt }} {{ target }} --ghc-options="-j" {{ flags }}

# build the target
build target="": (_do-build target "0")

# build the optimized target
build-opt target="": (_do-build target "2")

# clean the build files
clean:
    cabal clean

# run the solution for a day and a part
@run day part="0" input=("inputs/" + day + ".txt"): (build day)
    {{ join(build-path, "x/" + day + "/noopt/build/" + day + "/" + day) }} {{ part }} {{ input }}

# run the optimized solution for a day and a part
@run-opt day part="0" input=("inputs/" + day + ".txt"): (build-opt day)
    {{ join(build-path, "x/" + day + "/opt/build/" + day + "/" + day) }} {{ part }} {{ input }}

# run the solutions for both parts of all days
run-all opt="":
    just build{{ opt }}
    @for day in $(ls -d inputs/Day*.txt); do \
        day=$(basename -s ".txt" $day); \
        echo ""; \
        echo ">>> Running $day, part 1"; \
        just --no-deps run{{ opt }} "$day" 1; \
        echo ""; \
        echo ">>> Running $day, part 2"; \
        just --no-deps run{{ opt }} "$day" 2; \
        echo ""; \
    done
