# Repository preparation notes

Prepared from the local V5 workflow for public GitHub release.

Checks applied:

- Script paths are repository-relative.
- Local fallback paths outside `input/` were removed from the public copies.
- The Step 05 source dependency now points to `Step_04_Tune_Models_Expanding_Window.R`.
- Input data and generated outputs are excluded by `.gitignore`.
- Output folder placeholders are retained with `.gitkeep` files.
- The regressor manifest (`table_20_regressor_manifest.csv`) is treated as generated output: it is recreated by Step 03 and should not be committed.


