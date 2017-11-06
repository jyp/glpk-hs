

# Start ghci

    nix-shell -A glpk-hs.env release.nix --run 'cabal configure && ghci -isrc'

# Build

    nix-build release.nix

# Run example

```
result/bin/glpk-hs-example
0: obj =  -0.000000000e+00 inf =   0.000e+00 (3)
3: obj =   7.000000000e+02 inf =   0.000e+00 (0)
3: mip =     not found yet <=              +inf        (1; 0)
3: >>>>>   7.000000000e+02 <=   7.000000000e+02   0.0% (1; 0)
3: mip =   7.000000000e+02 <=     tree is empty   0.0% (0; 1)
(Success,Just (700.0,fromList [("x1",40.0),("x2",50.0),("x3",0.0)]))
```
