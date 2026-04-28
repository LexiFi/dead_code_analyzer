#!/bin/bash

# Update test scenarios' .ref files to the current results

make -C check stats

echo "COPYING"
cp check/threshold-3-0.5.out check/threshold-3-0.5/threshold-3-0.5.ref
cp check/threshold-1.out check/threshold-1/threshold-1.ref
cp check/internal.out check/internal/internal.ref
cp check/classic.out check/classic/classic.ref
echo "DONE"
