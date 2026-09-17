# CalculiX 2.23 (UB31 & Eurocode 3 Edition) — Standalone Builds

This release package contains standalone executable builds of **CalculiX CCX 2.23** enhanced with:
- **UB31 User Beam Element**: 2-node 3D Timoshenko / Euler-Bernoulli beam formulation with full 6 DOFs per node, 3D offsets, arbitrary end releases, and exact internal force recovery.
- **Eurocode 3 (EN 1993-1-1:2005) Code-Checking Engine (`*USER BEAM CHECK`)**: Real-time cross-section (Class 1–4), axial buckling ($\chi_y, \chi_z$), lateral-torsional buckling ($\chi_{LT}, M_{b,Rd}$), and interaction checks (Eq. 6.61 / 6.62).
- **Dynamic Station Subdivisions (`SUBDIVISIONS=<N>`)**: Flexible station sampling along beam spans for both force recovery and code checking.
- **UCONN6 Multi-DOF Nonlinear Spring/Connector Element**: Independent 6-DOF springs with ASCE 41-17 / FEMA 356 hysteretic plastic hinges.
- **Load Combinations Engine (`*USER LOAD COMBINATION`)**: Automated linear combinations of load cases.

---

## Package Contents & Directory Structure

```
Release/
├── README.md                          <-- This guide
├── linux/
│   ├── ccx_2.23                       <-- Linux x86_64 standalone binary (7.0 MB stripped)
│   └── ccx_2.23_linux_x86_64.tar.gz   <-- Compressed Linux binary archive (3.0 MB)
├── windows/
│   ├── ccx_2.23.exe                   <-- Windows x64 standalone executable (7.5 MB stripped)
│   └── ccx_2.23_windows_x64.zip       <-- Compressed Windows executable archive (3.1 MB)
└── examples/
    ├── cantilever_ec3.inp             <-- Sample cantilever beam with EC3 check
    └── ...
```

---

## How to Run

### Linux (x86_64)

1. Make the binary executable (if extracting from archive):
   ```bash
   chmod +x Release/linux/ccx_2.23
   ```
2. Run your input deck (without `.inp` extension):
   ```bash
   ./Release/linux/ccx_2.23 jobname
   ```

### Windows (x64)

1. Open Command Prompt (`cmd.exe`) or PowerShell in your working directory.
2. Run your input deck (without `.inp` extension):
   ```cmd
   ccx_2.23.exe jobname
   ```

---

## Quick Start Example

A ready-to-run cantilever beam deck is located in [`Release/examples/cantilever_ec3.inp`](examples/cantilever_ec3.inp).

```inp
*HEADING
Cantilever Beam with Eurocode 3 (EN 1993-1-1) Code Check
*NODE, NSET=NALL
1, 0.0, 0.0, 0.0
2, 6.0, 0.0, 0.0
*USER ELEMENT, TYPE=UB31, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
*ELEMENT, TYPE=UB31, ELSET=EBEAM
1, 1, 2
*USER BEAM SECTION, ELSET=EBEAM, MATERIAL=STEEL, SECTION=I
0.30, 0.15, 0.0107, 0.15, 0.0107, 0.0071
0.0, 1.0, 0.0
*MATERIAL, NAME=STEEL
*ELASTIC
210.0E9, 0.30
*BOUNDARY
1, 1, 6, 0.0
*USER BEAM DESIGN, ELSET=EBEAM
FY=275.0E6, LCR_Y=6.0, LCR_Z=6.0, L_LT=6.0, C1=1.13
*USER BEAM OUTPUT, FILE=cantilever_forces.csv, SUBDIVISIONS=5
*USER BEAM CHECK, CODE=EC3, STEEL=S275, SUBDIVISIONS=5, FILE=cantilever_ec3_check.csv
*STEP
*STATIC
*CLOAD
2, 2, -25000.0
*NODE FILE
U
*EL FILE
S
*END STEP
```

To run this example:
```bash
# On Linux:
../../Release/linux/ccx_2.23 cantilever_ec3

# On Windows:
..\windows\ccx_2.23.exe cantilever_ec3
```

This will produce:
- `cantilever_forces.csv`: Internal forces ($N, V_y, V_z, T, M_y, M_z$) at 6 stations along the element.
- `cantilever_ec3_check.csv`: Full Eurocode 3 utilization check ratios ($UC_{AX}, UC_{SH}, UC_{BND}, UC_{LTB}, UC_{STAB}, UC_{MAX}$) and governing failure mode for each station.
- Standard CalculiX results: `cantilever_ec3.frd`, `cantilever_ec3.dat`, `cantilever_ec3.sta`.

---

## Keyword Reference Summary

### 1. `*USER BEAM SECTION`
Defines the cross-section dimensions and orientation for UB31 elements:
```inp
*USER BEAM SECTION, ELSET=<name>, MATERIAL=<mat>, SECTION=<I|RECT|CIRC|PIPE|BOX|T|CHAN|L>
<dim1>, <dim2>, <dim3>, <dim4>, <dim5>, <dim6>
<n1>, <n2>, <n3>
```

### 2. `*USER BEAM DESIGN`
Specifies design parameters for code checking:
```inp
*USER BEAM DESIGN, ELSET=<name>
FY=355.0E6, LCR_Y=6.0, LCR_Z=6.0, L_LT=6.0, C1=1.13, KC=0.94, ALPHA_LT=0.34
```

### 3. `*USER BEAM OUTPUT`
Requests CSV output of internal forces along beam spans:
```inp
*USER BEAM OUTPUT, FILE=forces.csv, SUBDIVISIONS=10
```

### 4. `*USER BEAM CHECK`
Performs Eurocode 3 (EN 1993-1-1) member checks:
```inp
*USER BEAM CHECK, CODE=EC3, STEEL=S355, SUBDIVISIONS=10, FILE=codecheck.csv, OUTPUT=ALL
```
- `CODE=EC3` (Eurocode 3 EN 1993-1-1:2005)
- `STEEL=<S235|S275|S355|S420|S460>` (or custom yield strength via `FY` in `*USER BEAM DESIGN`)
- `SUBDIVISIONS=<N>`: Station count along span (default: inherits from `*USER BEAM OUTPUT` or 1)
- `FILE=<filename.csv>`: Output CSV file name
- `OUTPUT=<ALL|FAILED>`: Output all members or only overstressed members ($UC > 1.0$)
