#!/usr/bin/env bash
set -euo pipefail

# Run from the repository root after `stack build dietpdf:lib`.
# BASELINE_REV selects the Git revision compared with the working tree.
if (( $# == 0 )); then
  echo "Usage: bash bench/remove-unused-resources.sh PDF..." >&2
  exit 1
fi
benchmark_dir=$(mktemp -d)
trap 'rm -rf "$benchmark_dir"' EXIT
git show "${BASELINE_REV:-HEAD}:src/PDF/Document/Resources.hs" |
  sed 's/module PDF.Document.Resources/module BaselineResources/' > "$benchmark_dir/BaselineResources.hs"
sed 's/module PDF.Document.Resources/module OptimizedResources/' \
  src/PDF/Document/Resources.hs > "$benchmark_dir/OptimizedResources.hs"
stack exec -- ghc -O2 -XStrict -XOverloadedStrings -XFlexibleContexts \
  -XImportQualifiedPost -i"$benchmark_dir" -outputdir "$benchmark_dir" \
  bench/RemoveUnusedResources.hs -o "$benchmark_dir/benchmark" \
  -hide-all-packages -package base -package bytestring-0.11.5.4 \
  -package containers -package mtl -package transformers -package dietpdf
"$benchmark_dir/benchmark" "$@"
