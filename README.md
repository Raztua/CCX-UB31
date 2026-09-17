# CalculiX CCX 2.23 — UB31 Beam Element & User Sections Extension

[![CalculiX](https://img.shields.io/badge/CalculiX-CCX%202.23-blue.svg)](http://www.calculix.de/)
[![Element](https://img.shields.io/badge/Element-UB31-green.svg)]()
[![Kinematics](https://img.shields.io/badge/Formulation-Timoshenko%20%2F%20Euler--Bernoulli-orange.svg)]()
[![Validation](https://img.shields.io/badge/Validation-100%25%20Verified-brightgreen.svg)]()
[![License](https://img.shields.io/badge/License-GPL%20v2-lightgrey.svg)](CalculiX/ccx_2.23/src/gpl.htm)

A native extension and patch for **CalculiX CCX 2.23** adding the **UB31 (2-node 3D Timoshenko / Euler-Bernoulli user beam element)** and **User Beam Sections** system.

This implementation provides high-accuracy 3D beam modeling with complete rotational coupling, 8 cross-section shapes, member end releases (hinges), 3D geometric nodal offsets, rich distributed load distributions, mass formulation choices, and enhanced post-processing in **CalculiX GraphiX (CGX)**.

---

## 📑 Table of Contents
- [Key Features](#-key-features)
- [Repository Structure](#-repository-structure)
- [Installation Guide](#-installation-guide)
  - [Automated 1-Command Installation](#option-1-automated-1-command-installation-recommended)
  - [Manual Installation (Linux / macOS)](#option-2-manual-installation-linux--macos)
  - [Windows Installation (WSL / Git Bash / MSYS2)](#option-3-windows-installation)
- [Quickstart Example Deck](#-quickstart-example-deck)
- [Input Syntax & Usage](#-input-syntax--usage)
  - [1. User Element Declaration (`*USER ELEMENT`)](#1-user-element-declaration-user-element)
  - [2. User Beam Section (`*USER BEAM SECTION`)](#2-user-beam-section-user-beam-section)
  - [3. Raw Property Vector (`*USER SECTION, CONSTANTS=19`)](#3-raw-property-vector-user-section-constants19)
  - [4. Cross-Section Types & Parameters](#4-cross-section-types--parameters)
  - [5. Member End Releases (Hinges)](#5-member-end-releases-hinges)
  - [6. Distributed Loading Library (`*DLOAD`)](#6-distributed-loading-library-dload)
  - [7. Dynamic Multi-Station Beam CSV Output (`*USER BEAM OUTPUT`)](#7-dynamic-multi-station-beam-csv-output-user-beam-output)
  - [8. `UCONN6` Connectors & ASCE 41-17 Plastic Hinge Output (`*USER CONNECTOR OUTPUT`)](#8-uconn6-connectors--asce-41-17-plastic-hinge-output-user-connector-output)
  - [9. Native Step-Level Load Combinations (`*USER LOAD COMBINATION`)](#9-native-step-level-load-combinations-user-load-combination)
  - [10. Structural Steel Beam Code Checking (`*USER BEAM CHECK`)](#10-structural-steel-beam-code-checking-user-beam-check)
- [Analysis Capabilities](#-analysis-capabilities)
- [Post-Processing & CGX Visualization](#-post-processing--cgx-visualization)
- [Validation & Verification](#-validation--verification)
- [Documentation Reference](#-documentation-reference)
- [License](#-license)

---

## 🚀 Key Features

- **Element Formulation**: 2-node 3D beam element (`UB31`) with 6 DOFs per node (`UX`, `UY`, `UZ`, `ROTX`, `ROTY`, `ROTZ`).
- **Timoshenko Shear & Limiting Euler-Bernoulli Kinematics**: Exact shear coefficients computed automatically based on cross-section geometry and Poisson's ratio $\nu$.
- **8 Cross-Section Profiles**: `RECT`, `CIRC`, `PIPE`, `I`, `T`, `CHAN` (U-channel), `L` (Angle), and `BOX` (Hollow Box).
- **Asymmetric Section Handling**: Automatic determination of principal inertia axes ($I_{yy}$, $I_{zz}$) and principal rotation angle $\theta_p$ for `L` and `CHAN` sections to eliminate spurious bending-shear coupling.
- **Member End Releases (Hinges)**: Static condensation of rotational degrees of freedom at Node 1 and Node 2 (`ALLM`, `M1-M2`, `M1`, `M2`, `T`) or full 6-DOF bitwise fixity masks (`1..63`).
- **3D Geometric Nodal Offsets**: Eccentric neutral-axis shifts at element ends via `OFFSET=`, `OFFSET1=`, `OFFSET2=`.
- **Comprehensive Distributed Loading**: Uniform, triangular, trapezoidal, and partial patch transverse loads (`PX`, `P1`, `P2`, `P1_T1`, `P1_T2`, `P2_T1`, `P2_T2`, `P1_P_aa_bb`, `P2_P_aa_bb`), plus `CENTRIF` and `GRAV`.
- **Dynamic Mass Options**: Consistent mass matrix and optional lumped mass formulation (controlled by explicit dynamics or `CCX_LUMPED_MASS=1`).
- **Advanced Post-Processing**:
  - Automatically expands each UB31 element into 10 line sub-elements in the `.frd` file for smooth continuous stress and internal force contour visualization in CGX.
  - Generates 11-station internal force/stress evaluations per step in `ub31_beam_forces.csv`.

---

## 📁 Repository Structure

```text
CCX-CB/
├── CalculiX/                     # CalculiX CCX 2.23 source tree with SPOOLES & ARPACK
│   └── ccx_2.23/src/             # CCX core routines and patched Fortran/C modules
├── cgx_2.23.all/                 # CalculiX GraphiX (CGX 2.23) source tree
├── ub31_source_files/            # Pure Fortran modules for the UB31 patch
├── ub31_ccx223.patch             # Unified diff patch for a clean CCX 2.23 tree
├── install_ub31.sh               # Automated installer & compiler script
├── run_tests.py                  # Unified master test runner CLI
├── UB31_CCX223_Manual.md         # Comprehensive User Manual, Theory & Deck Reference
├── CGX_UB31_Guide.md             # CGX Visualization & Post-Processing Guide
├── README.md                     # Project README
└── tests/                        # Organized verification, validation & benchmark suites
    ├── 01_static_linear/         # Linear static, member releases & semi-rigid springs
    ├── 02_modal_eigenfrequency/  # Free vibration modal & natural frequency benchmarks
    ├── 03_geometric_nonlinear_pdelta/# Second-order P-Delta & geometric nonlinearity
    ├── 04_pushover_and_plasticity/# Inelastic pushover (ASCE 41-17, MDPI CBF, PyNite)
    ├── 05_full_verification_and_qa/# 10-Batch analytical verification & ground truth QA
    ├── 06_demos/                 # Mixed-dimensional demo models
    ├── reports/                  # Markdown verification reports
    └── run_all_tests.py          # Master test harness
```

---

## 🛠 Installation Guide

### Option 1: Automated 1-Command Installation (Recommended)

1. Make `install_ub31.sh` executable:
   ```bash
   chmod +x install_ub31.sh
   ```
2. Execute the script targeting your CCX source tree:
   ```bash
   ./install_ub31.sh CalculiX/ccx_2.23/src
   ```
   The script automatically detects directory depth, applies `ub31_ccx223.patch`, builds with `make -j$(nproc)`, and verifies the generated `ccx_2.23` binary.

---

### Option 2: Manual Installation (Linux / macOS)

From the root of a clean CalculiX 2.23 source tree:

```bash
# 1. Apply the patch
patch -p1 < ub31_ccx223.patch

# 2. Compile CCX binary
cd CalculiX/ccx_2.23/src
make -j$(nproc)
```

---

### Option 3: Windows Installation

- **WSL (Ubuntu / Debian - Recommended)**:
  ```bash
  sudo apt update && sudo apt install build-essential patch gfortran liblapack-dev libspooles-dev
  ./install_ub31.sh /mnt/c/path/to/ccx_2.23/src
  ```
- **Git Bash (Windows Native)**:
  ```bash
  patch -p1 < ub31_ccx223.patch
  ```
- **MSYS2 / MinGW-w64**:
  ```bash
  pacman -S patch make mingw-w64-x86_64-gfortran mingw-w64-x86_64-gcc
  cd CalculiX/ccx_2.23/src && make -f Makefile
  ```

---

## ⚡ Quickstart Example Deck

Create a file `cantilever.inp`:

```inp
*HEADING
UB31 Cantilever Beam - Static Point & Uniform Load
*USER ELEMENT, TYPE=UB31, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
*NODE, NSET=NALL
1,  0.0, 0.0, 0.0
2,  1.0, 0.0, 0.0
3,  2.0, 0.0, 0.0
4,  3.0, 0.0, 0.0
5,  4.0, 0.0, 0.0
6,  5.0, 0.0, 0.0
*ELEMENT, TYPE=UB31, ELSET=EBEAM
1, 1, 2
2, 2, 3
3, 3, 4
4, 4, 5
5, 5, 6
*MATERIAL, NAME=STEEL
*ELASTIC
2.1E11, 0.3
*DENSITY
7850.0
*USER BEAM SECTION, ELSET=EBEAM, MATERIAL=STEEL, SECTION=RECT
0.1, 0.2
*BOUNDARY
1, 1, 6
*STEP
*STATIC
*DLOAD
EBEAM, P1, -5000.0
*CLOAD
6, 2, -10000.0
*NODE PRINT, NSET=NALL
U, RF
*EL PRINT, ELSET=EBEAM
S
*END STEP
```

Run with CCX:
```bash
ccx_2.23 cantilever
```

---

## 📖 Input Syntax & Usage

### 1. User Element Declaration (`*USER ELEMENT`)
Before defining any UB31 elements, declare the user element type:
```inp
*USER ELEMENT, TYPE=UB31, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
```

#### Example Usage in Model Deck:
```inp
*NODE, NSET=NALL
1,   0.0, 0.0, 0.0
2,   3.0, 0.0, 0.0
3,   6.0, 0.0, 0.0

*USER ELEMENT, TYPE=UB31, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
*ELEMENT, TYPE=UB31, ELSET=EBEAM
1, 1, 2
2, 2, 3
```

---

### 2. User Beam Section (`*USER BEAM SECTION`)
High-level keyword for assigning geometry, orientation, releases, and offsets:

```inp
*USER BEAM SECTION, ELSET=<elset>, MATERIAL=<mat>, SECTION=<shape> [, ROTATION=<deg>] [, RELEASE1=<code>] [, RELEASE2=<code>] [, OFFSET1=(x,y,z)] [, OFFSET2=(x,y,z)]
<dim_1>, <dim_2>, <dim_3>, <dim_4>, <dim_5>, <dim_6>
[<e2_x>, <e2_y>, <e2_z>]
```
- **Data Line 1 (Required)**: Cross-section dimensions (`dims(1..6)`).
- **Data Line 2 (Optional)**: Local transverse orientation vector $\mathbf{e}_2$. If omitted, CCX-UB31 automatically calculates the upright normal orientation vector perpendicular to the beam axis.
- **Nodal Offsets**: Configured on the keyword line via `OFFSET=`, `OFFSET1=`, `OFFSET2=`.

#### Example A: Standard Rectangular Beam (Automatic Orientation)
```inp
*MATERIAL, NAME=STEEL
*ELASTIC
210.0E9, 0.30

*USER BEAM SECTION, ELSET=EBEAM, MATERIAL=STEEL, SECTION=RECT
0.15, 0.30
```

#### Example B: I-Beam Girder with Member End Releases (Automatic Orientation)
```inp
*USER BEAM SECTION, ELSET=EGIRDER, MATERIAL=STEEL, SECTION=I, RELEASE1=M1-M2, RELEASE2=M1-M2
0.300, 0.150, 0.0107, 0.150, 0.0107, 0.0071
```

#### Example C: Custom Non-Standard Orientation Vector (When Specific Orientation is Required)
```inp
*USER BEAM SECTION, ELSET=EGIRDER, MATERIAL=STEEL, SECTION=I, ROTATION=45.0
0.300, 0.150, 0.0107, 0.150, 0.0107, 0.0071
0.0, 0.0, 1.0
```

#### Example D: Hollow Box Section with 3D Geometric Nodal Offsets
```inp
*USER BEAM SECTION, ELSET=EBOX, MATERIAL=STEEL, SECTION=BOX, OFFSET1=(0.0, 0.10, 0.0), OFFSET2=(0.0, 0.10, 0.0)
0.20, 0.10, 0.008, 0.008, 0.008, 0.008
```

---

### 3. Raw Property Vector (`*USER SECTION, CONSTANTS=19`)
Generic CalculiX property array format where all 19 constant slots are passed across data lines:

```inp
*USER SECTION, ELSET=BEAM, MATERIAL=STEEL, CONSTANTS=19
1, 0.1, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0,
0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
0, 0,
0.0, 0.0, 0.0
```
- **Line 1 (Slots 1..8)**: `sect_type, dim1..dim6, rot_angle`
- **Line 2 (Slots 9..14)**: `off_x1, off_y1, off_z1, off_x2, off_y2, off_z2`
- **Line 3 (Slots 15..16)**: `rel_1, rel_2` (Bitmask integers: `48` = M1-M2, `56` = ALLM)
- **Line 4 (Slots 17..19)**: `e2_x, e2_y, e2_z` (Optional orientation vector; `0.0, 0.0, 0.0` uses auto-orientation)

#### Annotated Raw Vector Example:
```inp
*USER SECTION, ELSET=ECOLUMNS, MATERIAL=STEEL, CONSTANTS=19
** Line 1: Type (4=I-section), h=0.30, b_top=0.15, t_f1=0.0107, b_bot=0.15, t_f2=0.0107, t_w=0.0071, rot=0.0
4, 0.30, 0.15, 0.0107, 0.15, 0.0107, 0.0071, 0.0,
** Line 2: Nodal offsets (off_x1, off_y1, off_z1, off_x2, off_y2, off_z2)
0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
** Line 3: Releases at Node 1 and Node 2 (0 = Fully Fixed, 48 = Spherical Moment Release)
0, 48,
** Line 4: Transverse orientation vector e2 (0.0, 0.0, 0.0 = auto-orientation)
0.0, 0.0, 0.0
```

---

### 4. Cross-Section Types & Parameters

| Section Code (`SECTION=`) | Parameter List | Parameter Description |
| :--- | :--- | :--- |
| **`RECT`** | `b, h` | Width `b` (local $y$), Height `h` (local $z$) |
| **`CIRC`** | `r_o` | Outer radius $r_o$ (or `r_o, 0.0`) |
| **`PIPE`** | `r_o, t` | Outer radius $r_o$, Wall thickness $t$ |
| **`I`** | `h, b_top, t_f1, b_bot, t_f2, t_w` | Total height $h$, Top flange width/thickness, Bottom flange width/thickness, Web thickness |
| **`T`** | `h, b, t_f, t_w` | Total height $h$, Flange width $b$, Flange thickness $t_f$, Web thickness $t_w$ |
| **`CHAN`** (U-channel) | `h, b, t_f, t_w` | Channel height $h$, Flange width $b$, Flange thickness $t_f$, Web thickness $t_w$ |
| **`L`** (Angle) | `b, h, t` | Horizontal leg width $b$, Vertical leg height $h$, Thickness $t$ |
| **`BOX`** (Hollow Box) | `h, b, t_bot, t_left, t_top, t_right` | Total height $h$, Width $b$, Flange and web thicknesses (or `h, b, t, t, t, t`) |

#### Complete Input Examples for All 8 Cross-Section Shapes (Automatic Orientation):

```inp
** 1. Solid Rectangular: b = 0.15 m (width), h = 0.30 m (height)
*USER BEAM SECTION, ELSET=E_RECT, MATERIAL=STEEL, SECTION=RECT
0.15, 0.30

** 2. Solid Circular: r_o = 0.05 m (outer radius)
*USER BEAM SECTION, ELSET=E_CIRC, MATERIAL=STEEL, SECTION=CIRC
0.05

** 3. Hollow Pipe: r_o = 0.10 m (outer radius), t = 0.008 m (wall thickness)
*USER BEAM SECTION, ELSET=E_PIPE, MATERIAL=STEEL, SECTION=PIPE
0.10, 0.008

** 4. I-Beam (HEA 300): h=0.290, b_top=0.300, t_f1=0.014, b_bot=0.300, t_f2=0.014, t_w=0.0085
*USER BEAM SECTION, ELSET=E_IBEAM, MATERIAL=STEEL, SECTION=I
0.290, 0.300, 0.014, 0.300, 0.014, 0.0085

** 5. Tee Profile: h=0.150 m, b=0.100 m, t_f=0.010 m, t_w=0.006 m
*USER BEAM SECTION, ELSET=E_TEE, MATERIAL=STEEL, SECTION=T
0.150, 0.100, 0.010, 0.006

** 6. U-Channel (UPN 200): h=0.200 m, b=0.075 m, t_f=0.0115 m, t_w=0.0085 m
*USER BEAM SECTION, ELSET=E_CHAN, MATERIAL=STEEL, SECTION=CHAN
0.200, 0.075, 0.0115, 0.0085

** 7. Equal / Unequal Angle: b=0.100 m (leg 1), h=0.100 m (leg 2), t=0.010 m (thickness)
*USER BEAM SECTION, ELSET=E_ANGLE, MATERIAL=STEEL, SECTION=L
0.100, 0.100, 0.010

** 8. Rectangular Hollow Box (RHS): h=0.200, b=0.100, t_bot=0.008, t_left=0.008, t_top=0.008, t_right=0.008
*USER BEAM SECTION, ELSET=E_BOX, MATERIAL=STEEL, SECTION=BOX
0.200, 0.100, 0.008, 0.008, 0.008, 0.008
```

---

### 5. Member End Releases (Hinges)

Hinges can be defined using mnemonic string shortcuts or bitwise integers:

| Mnemonic Code | Description | Released DOFs | Bit Value |
| :--- | :--- | :---: | :---: |
| **`ALLM`** | **Full Moment / Ball-Joint + Torsion** | Local $R_x, R_y, R_z$ | **`56`** |
| **`M1-M2`** | **Spherical Bending Hinge** (pins both bending axes) | Local $R_y, R_z$ | **`48`** |
| **`M1`** | **Planar Bending Hinge about axis 1** ($y$) | Local $R_y$ | **`16`** |
| **`M2`** | **Planar Bending Hinge about axis 2** ($z$) | Local $R_z$ | **`32`** |
| **`T`** | **Torsional Pin** | Local $R_x$ | **`8`** |
| *Custom Integer* | *Sum of bit weights ($u_x=1, u_y=2, u_z=4, r_x=8, r_y=16, r_z=32$)* | Custom | `1..63` |

#### Example A: Pinned-Pinned Beam (Spherical Bending Hinges at Both Ends)
```inp
*USER BEAM SECTION, ELSET=EGIRDER, MATERIAL=STEEL, SECTION=I, RELEASE1=M1-M2, RELEASE2=M1-M2
0.30, 0.15, 0.0107, 0.15, 0.0107, 0.0071
```

#### Example B: Propped Cantilever with Tip Moment Release (Fixed at Node 1, Pinned at Node 2)
```inp
*USER BEAM SECTION, ELSET=EPROPPED, MATERIAL=STEEL, SECTION=RECT, RELEASE2=M2
0.10, 0.20
```

#### Example C: Space Truss Member (Ball Joints at Both Ends via Bitwise Masks)
```inp
*USER BEAM SECTION, ELSET=ETRUSS, MATERIAL=STEEL, SECTION=PIPE, RELEASE1=56, RELEASE2=56
0.06, 0.005
```

---

### 6. Distributed Loading Library (`*DLOAD`)

| Label | Description |
| :--- | :--- |
| **`PX`** | Uniform axial force per unit length along element axis. |
| **`P1`** | Uniform transverse load per unit length along local $y$. |
| **`P2`** | Uniform transverse load per unit length along local $z$. |
| **`P1_T1` / `P1_T2`** | Triangular load along local $y$ (increasing Node 1 $\rightarrow$ 2 / decreasing Node 1 $\rightarrow$ 2). |
| **`P2_T1` / `P2_T2`** | Triangular load along local $z$ (increasing Node 1 $\rightarrow$ 2 / decreasing Node 1 $\rightarrow$ 2). |
| **`P1_P_aa_bb`** | Partial patch load along local $y$ starting at `aa`% and ending at `bb`% of length. |
| **`P2_P_aa_bb`** | Partial patch load along local $z$ starting at `aa`% and ending at `bb`% of length. |
| **`CENTRIF`** | Centrifugal load field with rotational velocity $\omega$ and axis. |
| **`GRAV`** | Gravity/body accelerational force computed from material density $\rho$ and area $A$. |

#### Complete Distributed Loading Step Examples:

```inp
*STEP
*STATIC

*DLOAD
** 1. Uniform line load along local y (P1) and local z (P2)
EBEAMS, P1, -5000.0
EBEAMS, P2, -12000.0

** 2. Linearly varying (triangular) loads:
** P2_T1 ramps linearly from 0 at Node 1 to -8000 N/m at Node 2
EGIRDER, P2_T1, -8000.0
** P1_T2 starts at -6000 N/m at Node 1 and drops linearly to 0 at Node 2
EWALL, P1_T2, -6000.0

** 3. Partial patch loads:
** P2_P_25_75 applies -15000 N/m strictly between 25% and 75% of member span
ESPAN, P2_P_25_75, -15000.0

** 4. Uniform axial traction per unit length along beam axis (PX)
ECOLUMNS, PX, -2000.0

** 5. Gravity / Self-Weight (computes rho * A * g automatically)
EALL_BEAMS, GRAV, 9.81, 0.0, -1.0, 0.0

** 6. Centrifugal Force (rotational speed rad/s and axis vector)
EROTOR, CENTRIF, 314.159, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0

*NODE FILE
U, RF
*EL FILE
S
*END STEP
```

---

### 7. Dynamic Multi-Station Beam CSV Output (`*USER BEAM OUTPUT`)

Zero-RAM, high-performance streaming of internal beam results along member spans directly to CSV:

```inp
*USER BEAM OUTPUT, FILE=girders.csv, ELSET=EGIRDERS, SUBDIVISIONS=10, INCREMENT=LAST
F, U, S
```

#### Parameters:
- **`FILE=`**: Destination filename (e.g. `girders.csv`) or tuple list `FILE=(f1.csv, f2.csv)` mapped 1-to-1 with `ELSET=(...)`.
- **`ELSET=`**: Target element set(s) (e.g. `EBEAM`, `ELSET=(COLS, GIRDERS)`, `ELSET=ALL`, or `ELSET=*`).
- **`SUBDIVISIONS=N`**: Number of internal span evaluation stations per member ($N=1..100+$). Evaluates exact Hermite shape functions + closed-form particular sag $v_0(x), w_0(x)$ under distributed line loads.
- **`INCREMENT=`**: Output step filter (`LAST`, `ALL`, `FREQ=k`, or list `(1, 5, 10)`).
- **`COORDINATES=`**: Coordinate transformation system (`LOCAL` or `GLOBAL`).

#### Column Variable Selectors:
- **`F`**: Internal forces & moments (`Fx_Axial, Vy_Shear, Vz_Shear, Mx_Torsion, My_Bending, Mz_Bending`).
- **`U`**: 3D Displacements and cross-section rotations (`Ux, Uy, Uz, Rot_X, Rot_Y, Rot_Z`).
- **`S`**: Longitudinal and shear stresses (`Sxx_Axial, Sxx_Bending_Y, Sxx_Bending_Z, Sxx_Max_Combined, Sxy_Shear, Sxz_Shear, Stors_Torsion`).
- **`Q`**: Applied line load values (`Qx_Load, Qy_Load, Qz_Load`).
- **`ALL`**: All 26 standard columns.

#### Example A: Standard 10-Station Internal Results for Design
```inp
*STEP
*STATIC
*DLOAD
EBEAM, P2, -15000.0
*USER BEAM OUTPUT, FILE=beam_internal_forces.csv, ELSET=EBEAM, SUBDIVISIONS=10, INCREMENT=LAST
F, U, S
*END STEP
```

#### Example B: Multi-Set Multi-File Output for Girders and Columns
```inp
*USER BEAM OUTPUT, FILE=(girders_out.csv, columns_out.csv), ELSET=(EGIRDERS, ECOLUMNS), SUBDIVISIONS=5
F, S
```

#### Example C: Global Coordinate Transformation Across All Dynamic Increments
```inp
*USER BEAM OUTPUT, FILE=frame_history.csv, ELSET=EALL, COORDINATES=GLOBAL, INCREMENT=ALL
ALL
```

#### Generated CSV Format:
```csv
Step,Increment,Time,Element,Station_Pct,X_local,Fx_Axial,Vy_Shear,Vz_Shear,Mx_Torsion,My_Bending,Mz_Bending,Ux,Uy,Uz,Rot_X,Rot_Y,Rot_Z,...
```

---

### 8. `UCONN6` Connectors & ASCE 41-17 Plastic Hinge Output (`*USER CONNECTOR OUTPUT`)

6-DOF zero-length connector element (`UCONN6`) for discrete joint springs, member end releases, and nonlinear ASCE 41-17 plastic hinges:

#### A. Connector Definition (`*USER CONNECTOR`)

##### Example 1: Linear Elastic Rotational Spring (Semi-Rigid Joint)
Connect two coincident nodes with a rotational stiffness of $K_{\theta y} = 5.0 \times 10^6 \text{ N}\cdot\text{m/rad}$:
```inp
*NODE, NSET=NCONN
10,   4.0, 0.0, 0.0
101,  4.0, 0.0, 0.0

*USER ELEMENT, TYPE=UCONN6, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
*ELEMENT, TYPE=UCONN6, ELSET=ESEMIRIGID
50, 10, 101

*USER CONNECTOR, ELSET=ESEMIRIGID
1.0E12, 1.0E12, 1.0E12, 1.0E12, 5.0E6, 1.0E12
```

##### Example 2: Nonlinear ASCE 41-17 Plastic Hinge Backbone
Define an inelastic flexural plastic hinge on degree-of-freedom 5 ($R_y$ bending) with yield moment $M_y = 250 \text{ kN}\cdot\text{m}$, rotation limits, and post-yield strain hardening:
```inp
*ELEMENT, TYPE=UCONN6, ELSET=EHINGE
51, 10, 101

*USER CONNECTOR, ELSET=EHINGE, NONLINEAR=ASCE41
** Line 1: Elastic uncoupled stiffness for all 6 DOFs (DOF 5 is overridden by backbone)
1.0E12, 1.0E12, 1.0E12, 1.0E12, 0.0, 1.0E12
** Line 2: My, theta_y, theta_cap, c_res, theta_u, theta_fail, alpha_hard, dof_idx
250.0E3, 0.005, 0.025, 0.20, 0.040, 0.050, 0.03, 5
```

#### B. Connector Output Card (`*USER CONNECTOR OUTPUT`)
```inp
*STEP, NLGEOM
*STATIC
*CLOAD
101, 2, -50000.0
*USER CONNECTOR OUTPUT, FILE=hinge_results.csv, ELSET=EHINGE, INCREMENT=ALL
F, U, STATE
*END STEP
```
- **`F`**: Connector forces & moments (`Fx, Fy, Fz, Mx, My, Mz`).
- **`U`**: Relative joint deformations (`dUx, dUy, dUz, dRotX, dRotY, dRotZ`).
- **`STATE`** / **`ASCE41`**: Damage state (`Elastic`, `IO`, `LS`, `CP`, `Failure`), `Yield_Ratio`, `Plastic_Def`, and `Tangent_K`.
- **`ALL`**: All connector columns.

#### Generated CSV Format:
```csv
Step,Increment,Time,Element,Node1,Node2,Fx,Fy,Fz,Mx,My,Mz,dUx,dUy,dUz,dRotX,dRotY,dRotZ,ASCE41_State,Yield_Ratio,Plastic_Def,Tangent_K
```

---

### 9. Native Step-Level Load Combinations (`*USER LOAD COMBINATION`)

Allows structural engineers to define basic primary load cases in early steps, and synthesize factored load combinations into separate analysis steps directly inside the CalculiX deck without external pre-/post-processing scripts.

#### Complete Multi-Step Working Example:
```inp
*HEADING
Multi-Case Building Frame with Native Factored Combinations

** --- Primary Load Cases ---
*STEP
*STATIC
** Step 1: Self-Weight / Dead Load (G)
*DLOAD
EALL_BEAMS, GRAV, 9.81, 0.0, -1.0, 0.0
*END STEP

*STEP
*STATIC
** Step 2: Imposed Live Load (Q)
*DLOAD
EGIRDERS, P2, -15000.0
*END STEP

*STEP
*STATIC
** Step 3: Lateral Wind Load (W)
*CLOAD
10, 1, 45000.0
20, 1, 45000.0
*END STEP

** --- Factored Design Combinations ---
*STEP
*STATIC
** Step 4: ULS Fundamental Combination (1.35*G + 1.50*Q + 0.90*W)
*USER LOAD COMBINATION
ULS_STR, 1, 1.35, 2, 1.50, 3, 0.90
*NODE FILE
U, RF
*EL FILE
S
*USER BEAM OUTPUT, FILE=forces_uls.csv, ELSET=EGIRDERS, SUBDIVISIONS=10
F, S
*END STEP

*STEP, NLGEOM
*STATIC
** Step 5: SLS Characteristic Combination (1.00*G + 1.00*Q) with Second-Order P-Delta
*USER LOAD COMBINATION
SLS_CHAR, 1, 1.00, 2, 1.00
*NODE FILE
U
*USER BEAM OUTPUT, FILE=forces_sls.csv, ELSET=EGIRDERS, SUBDIVISIONS=10
F, U
*END STEP
```

#### Supported Features:
- **Load Types Synthesized**: Factored combination of nodal point loads & moments (`*CLOAD`), element distributed line loads (`*DLOAD`), and volumetric/gravity loads (`*DLOAD, GRAV`).
- **Single or Multi-Line Continuation**: Define combinations on one line or split across multiple lines using an optional combination label (`ULS_STR`, `SLS_CHAR`, etc.).
- **True Nonlinear Solver Integration**: Factored loads are applied upfront before matrix assembly, fully compatible with `*STEP, NLGEOM` for second-order $P$-$\Delta$ equilibrium and stability calculations.
- **Native Post-Processing**: Displacements, reactions, stresses, and beam internal forces are written to native `.frd` datasets and CSV outputs for every combination step.

---

### 10. Structural Steel Beam Code Checking (`*USER BEAM CHECK`)

Native structural steel code-checking for **Eurocode 3 (EN 1993-1-1)** and **AISC 360-16 / 360-22 LRFD**, supporting cross-section classification, member flexural/torsional buckling, combined axial-bending stability equations, multi-station CSV tabular output, and 3D color contour mapping in CGX (`UCHK`).

#### Complete Working Example Deck:
```inp
*HEADING
Eurocode 3 & AISC 360 Automated Member Code Checking

*USER ELEMENT, TYPE=UB31, NODES=2, MAXDOF=6, INTEGRATIONPOINTS=1
*NODE
1, 0.0, 0.0, 0.0
2, 6.0, 0.0, 0.0
*ELEMENT, TYPE=UB31, ELSET=EBEAM
1, 1, 2

*USER BEAM SECTION, ELSET=EBEAM, MATERIAL=STEEL, SECTION=I
0.300, 0.150, 0.0107, 0.150, 0.0107, 0.0071

*MATERIAL, NAME=STEEL
*ELASTIC
210.0E9, 0.30
*BOUNDARY
1, 1, 6
2, 2, 3

** --- Code Check Configuration ---
** Global member parameters (Yield strength, Buckling lengths Lcr_y, Lcr_z, L_LT, Moment factor C1)
*USER BEAM DESIGN, ELSET=EBEAM
FY=355.0E6, LCR_Y=6.0, LCR_Z=3.0, L_LT=3.0, C1=1.13

** Element-specific or set overrides:
*USER BEAM DESIGN OVERRIDES
** TARGET_ID,  LCR_Y,  LCR_Z,  L_LT,   C1,    FY
   1,          6.0,    3.0,    3.0,    1.13,  355.0E6

** Execute Eurocode 3 check card:
*USER BEAM CHECK, CODE=EC3, FILE=codecheck_ec3.csv, SUBDIVISIONS=5, OUTPUT=ALL

*STEP
*STATIC
*DLOAD
EBEAM, P2, -25000.0
*NODE FILE
U, RF
*EL FILE
S
*END STEP
```

#### Supported Standards & Checks:
- **Eurocode 3 (EN 1993-1-1:2005)**: Cross-section Classes 1–3, tension $N_{t,Rd}$, compression $N_{c,Rd}$, shear $V_{c,Rd}$, bending $M_{c,Rd}$ with high-shear reduction $M_{y,V,Rd}$, column flexural buckling $\chi_y, \chi_z$, lateral-torsional buckling $\chi_{LT}$, and Annex B stability interaction equations (Eq. 6.61 & 6.62 with $k_{yy}, k_{yz}, k_{zy}, k_{zz}$).
- **AISC 360-16 / 360-22 LRFD**: Chapter D (Tension), Chapter E (Column Buckling with $F_{cr}, \phi_c P_n$), Chapter F (Flexure & LTB with $M_p, L_p, L_r, \phi_b M_n$), Chapter G (Shear $\phi_v V_n$), and Chapter H combined interaction equations (Eq. H1-1a & H1-1b).
- **Post-Processing**: Summary terminal table, 11-station longitudinal CSV output, and native `.frd` dataset `UCHK` (`UCTOT`, `UCAX`, `UCSH`, `UCBND`, `UCSTAB`, `UCLTB`) for 3D color contour plotting in CGX.

---

## 🔬 Analysis Capabilities

The UB31 and UCONN6 extensions support all standard CCX step procedures:

### 1. Linear Static Analysis (`*STATIC`)
```inp
*STEP
*STATIC
*DLOAD
EBEAM, P2, -10000.0
*NODE FILE
U, RF
*EL FILE
S
*END STEP
```

### 2. Natural Frequency & Modal Analysis (`*FREQUENCY`)
Extracts the first 10 natural frequencies and 3D mode shapes:
```inp
*STEP
*FREQUENCY
10
*NODE FILE
U
*EL FILE
S
*END STEP
```

### 3. Linear Critical Eigenvalue Buckling (`*BUCKLE`)
Computes critical buckling factors and mode shapes using the exact UB31 geometric stiffness matrix:
```inp
*STEP
*BUCKLE
5
*CLOAD
2, 1, -1.0
*NODE FILE
U
*EL FILE
S
*END STEP
```

### 4. Direct Integration Dynamic Analysis (`*DYNAMIC` / `*MODAL DYNAMIC`)
Time-history dynamic response under transient time-varying loadings:
```inp
*STEP
*DYNAMIC, DIRECT
1.0E-4, 0.10
*DLOAD
EBEAM, P2, -5000.0
*NODE FILE, FREQUENCY=10
U
*USER BEAM OUTPUT, FILE=dynamic_history.csv, ELSET=EBEAM, INCREMENT=ALL
F, U
*END STEP
```

---

## 📊 Post-Processing & CGX Visualization

Visualizing results in **CalculiX GraphiX (CGX)**:

```bash
cgx cantilever.frd
```

### Essential CGX Commands
```cgx
read cantilever.frd      # Load result deck
view elem                # Show element boundaries
plot elem                # Render element mesh

# Displacements & Mode Shapes
ds 1 e 2                 # Select Dataset 1, Component 2 (UY)
ds 1 e 4                 # Select Total Magnitude (ALL)
plot f                   # Plot color contours
view disp                # Toggle deformed view
scal d 50                # Scale deformation display 50x

# Stresses & Internal Forces
ds 2 e 1                 # SXX (Max Normal Stress / Axial)
ds 2 e 2                 # SYY (Bending Moment My)
ds 2 e 3                 # SZZ (Bending Moment Mz)
plot f                   # Render contours
```

For complete step-by-step CGX batch scripting and dataset queries, see [`CGX_UB31_Guide.md`](CGX_UB31_Guide.md).

---

## ✅ Validation & Verification

The patch includes 15 automated validation suites verifying the implementation against theoretical analytical solutions (Timoshenko & Euler-Bernoulli beam theory) and Nastran 95 baselines:

### 1. Running the Verification Suites
```bash
# Run fast smoke tests across all categories (15 test suites)
python3 run_tests.py

# Run full multi-section verification across all shapes (RECT, BOX, CIRC, PIPE, L, I, T)
python3 run_tests.py --suite verif

# Run unit tests and member end releases
python3 run_tests.py --suite unit

# Run nonlinear pushover and frame benchmarks
python3 run_tests.py --suite pushover

# Run all test suites
python3 run_tests.py --all
```

### 2. Validation Results Summary
- **Static Deflection & Reactions**: **`0.000%` error** against exact Timoshenko closed-form solutions.
- **Dynamic Station Displacements**: Exact match with Euler-Bernoulli particular sag ($v_{\text{mid}} = \frac{5 w L^4}{384 E I}$).
- **Eigenfrequency Modal Analysis**: **`< 0.05%` error** against analytical beam natural frequencies.
- **Eigenvalue Buckling Analysis**: Exact match on multi-span frames and column stability limits.
- **15 / 15 Test Suites Passed (100% success)** across static, modal, P-Delta, ASCE 41-17 pushover, and QA verification suites.

Detailed benchmarks, pass/fail status breakdowns, and comparison tables are available in:
- **[`tests/reports/ANALYSIS_STATUS_SUMMARY.md`](tests/reports/ANALYSIS_STATUS_SUMMARY.md)**: Pass/Fail/Review/NA metrics and physical explanations across all analysis categories.
- **[`tests/reports/MASTER_RESULTS_SUMMARY.md`](tests/reports/MASTER_RESULTS_SUMMARY.md)**: Master quantitative comparison table across all 7 physical quantities, 5 analysis types, and reference baselines.
- **[`tests/reports/global_verification_report.md`](tests/reports/global_verification_report.md)**: Multi-section 10-batch analytical verification report across all 8 shapes.
- **[`tests/reports/validation_report.md`](tests/reports/validation_report.md)**: Static, modal, transient dynamic, and Nastran 95 validation report.
- **[`tests/reports/qa_quality_report.md`](tests/reports/qa_quality_report.md)**: QA robustness, equilibrium, energy conservation, and mechanism report.

---

## 📚 Documentation Reference

- **[`UB31_CCX223_Manual.md`](UB31_CCX223_Manual.md)**: Full technical reference manual, mathematical derivations, cross-section formulations, and verified example decks.
- **[`CGX_UB31_Guide.md`](CGX_UB31_Guide.md)**: Complete guide for post-processing and rendering UB31 results in CGX.
- **[`tests/README.md`](tests/README.md)**: Comprehensive test suite documentation, category organization, and execution guide.
- **[`tests/reports/ANALYSIS_STATUS_SUMMARY.md`](tests/reports/ANALYSIS_STATUS_SUMMARY.md)**: Comprehensive status metrics (Pass / Review / Fail / NA) breakdown by analysis type.
- **[`tests/reports/MASTER_RESULTS_SUMMARY.md`](tests/reports/MASTER_RESULTS_SUMMARY.md)**: Quantitative comparison tables vs Nastran 95, OpenSees, PyNite, LUSAS, and analytical theory.

---

## 📄 License

CalculiX is distributed under the terms of the **GNU General Public License (GPL v2)**. See the `CalculiX/ccx_2.23/src/gpl.htm` file for license details.
