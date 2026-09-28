#!/bin/bash

# Update test scenarios' .ref files to the current results

make -C check stats

scenarios="threshold-3-0.5 threshold-1 internal classic exclude"

echo "COPYING"
for scenario in $scenarios
do
  cp check/$scenario.out check/$scenario/$scenario.ref
done
echo "DONE"
