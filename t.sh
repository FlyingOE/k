#!/bin/bash
sed 's/ *\/= */\t/' t.k|while IFS='	' read -r x y; do
  z="$(./k <<<"$x")"
  if [ "$z" != "$y" ]; then echo "$x"; echo "  expected: $y"; echo "  actual:   $z"; fi
done
