#!/bin/bash
set -e

echo "=== Verifying GHC installation ==="
ghc --version
ghc-pkg list | grep -E "linear|lens|distributive|hmatrix|storable-complex" || echo "Warning: Some packages may not be installed"

echo ""
echo "=== Building Haskell modules ==="

# Build in dependency order
modules=(
  "Linear/Diagonal.hs"
  "Multiplicative/Class.hs"
  "Local.hs"
  "Data/FMonoid/Class.hs"
  "CartesianProduct.hs"
  "As.hs"
  "Rotation/SO2.hs"
  "Rotation/SO3.hs"
  "Rotation/Class.hs"
  "SemiProduct.hs"
  "Space/Class.hs"
  "Exponential/Class.hs"
  "Exponential/SO2.hs"
  "Exponential/SO3.hs"
  "MultiLinear/Class.hs"
  "Space/SO2.hs"
  "Space/SO3.hs"
  "Ellipsoid.hs"
  "Space/SE3.hs"
  "Scaling/S1.hs"
  "Scaling/Time.hs"
  "Solver/RungeKutta.hs"
  "Product.hs"
)

for module in "${modules[@]}"; do
  echo "Compiling $module..."
  if ! ghc -c "$module" 2>&1; then
    echo "ERROR: Failed to compile $module"
    exit 1
  fi
done

echo ""
echo "=== Build successful! ==="
echo "Generated object files:"
find . -name "*.o" -type f | wc -l
