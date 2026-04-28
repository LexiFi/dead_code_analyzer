#!/bin/bash

# Run the analyzer on a given project multiple times with different options.
# The runs are measured using `time --verbose` and the results are stored in the
# suggested output directory (if any) or in the current directory.

usage() {
  echo "Usage: $0 [-a dead_code_analyzer] [-o outdir] project"
  echo ""
  echo "Options:"
  echo "  -a dead_code_analyzer  Select the analyzer to use (default is _build/install/default/bin/dead_code_analyzer)"
  echo "  -o outdir  Write results files in directory outdir (default is .)"
  echo "  -h  Display this list of options"
}

# Default values

dca="_build/install/default/bin/dead_code_analyzer"
outdir="."

# Parse opt args

while getopts "a:o:h" OPT
do
  case "$OPT" in
    a) dca=$OPTARG ;;
    o) outdir=$OPTARG ;;
    h) usage; exit 0 ;;
  esac
done

# Parse positional args

shift $((OPTIND -1))

if [ "$#" -ne 1 ]
then
  usage
  exit 1
fi

proj="$1"

# Ensure the output dir exists

if [ ! -d "$outdir" ]
then
  mkdir -p "$outdir"
fi

# Benchamark dca on proj and store the results in outdir
# for each flags in name_and_flags

declare -A name_and_flags
name_and_flags["default"]=""
name_and_flags["all"]="--all"
name_and_flags["eall"]="-a -E all"
name_and_flags["mall"]="-a -M all"
name_and_flags["tall"]="-a -T all"
name_and_flags["oaall"]="-a -Oa all"
name_and_flags["onall"]="-a -On all"
name_and_flags["sall"]="-a -S +all"
name_and_flags["nothing"]="--nothing"
name_and_flags["verbose"]="-a --verbose"

for name in ${!name_and_flags[@]}
do
  echo 'running: `' "$dca" ${name_and_flags[$name]} "$proj" '`'
  \time --verbose "$dca" ${name_and_flags[$name]} "$proj" > "$outdir"/$name.out 2> "$outdir"/$name.err
done
